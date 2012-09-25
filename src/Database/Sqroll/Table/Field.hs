{-# LANGUAGE FlexibleInstances #-}
module Database.Sqroll.Table.Field
    ( Field (..)
    ) where

import Data.ByteString (ByteString)
import Data.Time (Day (..), UTCTime (..), formatTime, parseTime)
import Data.Int (Int64)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as B

import Database.Sqroll.Sqlite3

class Field a where
    fieldType    :: a -> SqlType  -- Should work with 'undefined'
    fieldIndex   :: a -> Bool     -- Should work with 'undefined'
    fieldDefault :: a
    fieldPoke    :: SqlStmt -> Int -> a -> IO ()
    fieldPeek    :: SqlStmt -> Int -> IO a

instance Field Int where
    fieldType    = const SqlInteger
    fieldIndex   = const False
    fieldDefault = 0

    fieldPoke stmt n = sqlBindInt64 stmt n . fromIntegral
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap fromIntegral (sqlColumnInt64 stmt n)
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
