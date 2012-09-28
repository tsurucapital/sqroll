{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Sqroll.Table.Field
    ( Field (..)
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
    fieldType    :: a -> SqlType  -- Should work with 'undefined'
    fieldIndex   :: a -> Bool     -- Should work with 'undefined'
    fieldDefault :: a
    fieldPoke    :: SqlStmt -> Int -> a -> IO ()
    fieldPeek    :: SqlStmt -> Int -> IO a

    -- Defaults: use Binary
    default fieldType :: a -> SqlType
    fieldType = const SqlBlob

    default fieldIndex :: a -> Bool
    fieldIndex = const False

    default fieldDefault :: Monoid a => a
    fieldDefault = mempty

    default fieldPoke :: Binary a => SqlStmt -> Int -> a -> IO ()
    fieldPoke stmt n x = sqlBindLazyByteString stmt n (encode x)

    default fieldPeek :: Binary a => SqlStmt -> Int -> IO a
    fieldPeek stmt n = fmap decode $ sqlColumnLazyByteString stmt n

instance Field Int where
    fieldType    = const SqlInteger
    fieldIndex   = const False
    fieldDefault = 0

    fieldPoke stmt n = sqlBindInt64 stmt n . fromIntegral
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap fromIntegral (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field Bool where
    fieldType    = const SqlInteger
    fieldIndex   = const False
    fieldDefault = False

    fieldPoke stmt n False = sqlBindInt64 stmt n 0
    fieldPoke stmt n True  = sqlBindInt64 stmt n 1
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap (/= 0) (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field Int64 where
    fieldType    = const SqlInteger
    fieldIndex   = const False
    fieldDefault = 0

    fieldPoke = sqlBindInt64
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnInt64
    {-# INLINE fieldPeek #-}

instance Field String where
    fieldType    = const SqlText
    fieldIndex   = const False
    fieldDefault = ""

    fieldPoke = sqlBindString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnString
    {-# INLINE fieldPeek #-}

instance Field Double where
    fieldType    = const SqlDouble
    fieldIndex   = const False
    fieldDefault = 0

    fieldPoke    = sqlBindDouble
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnDouble
    {-# INLINE fieldPeek #-}

instance Field SqlRowId where
    fieldType    = const SqlInteger
    fieldIndex   = const True
    fieldDefault = SqlRowId 0

    fieldPoke stmt n (SqlRowId x) = sqlBindInt64 stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap SqlRowId . sqlColumnInt64 stmt
    {-# INLINE fieldPeek #-}

instance Field ByteString where
    fieldType    = const SqlBlob
    fieldIndex   = const False
    fieldDefault = B.empty

    fieldPoke = sqlBindByteString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnByteString
    {-# INLINE fieldPeek #-}

instance Field BL.ByteString where
    fieldType    = const SqlBlob
    fieldIndex   = const False
    fieldDefault = BL.empty

    fieldPoke = sqlBindLazyByteString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnLazyByteString
    {-# INLINE fieldPeek #-}

instance Field UTCTime where
    fieldType    = const SqlText
    fieldIndex   = const False
    fieldDefault = UTCTime (ModifiedJulianDay 0) 0

    fieldPoke stmt n time = sqlBindString stmt n (formatSqliteTime time)
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap parseSqliteTime . sqlColumnString stmt
    {-# INLINE fieldPeek #-}

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
