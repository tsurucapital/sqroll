{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Sqroll.Table.Field
    ( Field (..)
    , fieldIndexed
    ) where

import Data.Binary (Binary, encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (Monoid, mempty)
import Data.Time (Day (..), UTCTime (..), formatTime, parseTime)
import Data.Int (Int64)
import System.Locale (defaultTimeLocale)

import Database.Sqroll.Sqlite3

class Field a where
    fieldType    :: a -> SqlType   -- Should work with 'undefined'
    fieldRefers  :: a -> [String]  -- Should work with 'undefined'
    fieldDefault :: a
    fieldPoke    :: SqlStmt -> Int -> a -> IO ()
    fieldPeek    :: SqlStmt -> Int -> IO a

    -- Defaults: use Binary
    default fieldType :: a -> SqlType
    fieldType = const SqlBlob

    default fieldRefers :: a -> [String]
    fieldRefers = const []

    default fieldDefault :: Monoid a => a
    fieldDefault = mempty

    default fieldPoke :: Binary a => SqlStmt -> Int -> a -> IO ()
    fieldPoke stmt n x = sqlBindLazyByteString stmt n (encode x)

    default fieldPeek :: Binary a => SqlStmt -> Int -> IO a
    fieldPeek stmt n = fmap decode $ sqlColumnLazyByteString stmt n

instance Field Int where
    fieldType    = const SqlInteger
    fieldDefault = 0

    fieldPoke stmt n = sqlBindInt64 stmt n . fromIntegral
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap fromIntegral (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field Bool where
    fieldType    = const SqlInteger
    fieldDefault = False

    fieldPoke stmt n False = sqlBindInt64 stmt n 0
    fieldPoke stmt n True  = sqlBindInt64 stmt n 1
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap (/= 0) (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field Int64 where
    fieldType    = const SqlInteger
    fieldDefault = 0

    fieldPoke = sqlBindInt64
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnInt64
    {-# INLINE fieldPeek #-}

instance Field String where
    fieldType    = const SqlText
    fieldDefault = ""

    fieldPoke = sqlBindString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnString
    {-# INLINE fieldPeek #-}

instance Field Double where
    fieldType    = const SqlDouble
    fieldDefault = 0

    fieldPoke    = sqlBindDouble
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnDouble
    {-# INLINE fieldPeek #-}

instance Field ByteString where
    fieldType    = const SqlBlob
    fieldDefault = B.empty

    fieldPoke = sqlBindByteString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnByteString
    {-# INLINE fieldPeek #-}

instance Field BL.ByteString where
    fieldType    = const SqlBlob
    fieldDefault = BL.empty

    fieldPoke = sqlBindLazyByteString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnLazyByteString
    {-# INLINE fieldPeek #-}

instance forall a. Field a => Field (Maybe a) where
    fieldType    = const $ fieldType (undefined :: a)
    fieldDefault = Nothing

    fieldPoke stmt n Nothing  = sqlBindNothing stmt n
    fieldPoke stmt n (Just x) = fieldPoke stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = do
        nothing <- sqlColumnIsNothing stmt n
        if nothing
            then return Nothing
            else fmap Just $ fieldPeek stmt n
    {-# INLINE fieldPeek #-}

instance Field UTCTime where
    fieldType    = const SqlText
    fieldDefault = UTCTime (ModifiedJulianDay 0) 0

    fieldPoke stmt n time = sqlBindString stmt n (formatSqliteTime time)
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap parseSqliteTime . sqlColumnString stmt
    {-# INLINE fieldPeek #-}

fieldIndexed :: Field a => a -> Bool
fieldIndexed = not . null . fieldRefers

formatSqliteTime :: UTCTime -> String
formatSqliteTime = take 23 . formatTime defaultTimeLocale sqliteTimeFmt
{-# INLINE formatSqliteTime #-}

parseSqliteTime :: String -> UTCTime
parseSqliteTime string =
    case parseTime defaultTimeLocale sqliteTimeFmt string of
        Just t  -> t
        Nothing -> error $ "parseSqliteTime: Could not parse: " ++ string
{-# INLINE parseSqliteTime #-}

sqliteTimeFmt :: String
sqliteTimeFmt = "%Y-%m-%d %H:%M:%S%Q"
{-# INLINE sqliteTimeFmt #-}
