{-# LANGUAGE FlexibleInstances #-}
module Database.Sqroll.Table.Field where

import Database.Sqroll.Table.Sqlite3

class Field a where
    fieldType  :: a -> String  -- Should work with 'undefined'
    fieldIndex :: a -> Bool    -- Should work with 'undefined'
    fieldPoke  :: SqlStmt -> Int -> a -> IO ()
    fieldPeek  :: SqlStmt -> Int -> IO a

instance Field Int where
    fieldType  = const "INTEGER"
    fieldIndex = const False

    fieldPoke stmt n = sqlBindInt64 stmt n . fromIntegral
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap fromIntegral (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field String where
    fieldType  = const "TEXT"
    fieldIndex = const False

    fieldPoke = sqlBindString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnString
    {-# INLINE fieldPeek #-}

instance Field Double where
    fieldType  = const "DOUBLE"
    fieldIndex = const False

    fieldPoke = sqlBindDouble
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnDouble
    {-# INLINE fieldPeek #-}

instance Field SqlRowId where
    fieldType  = const "INTEGER"
    fieldIndex = const True

    fieldPoke stmt n (SqlRowId x) = sqlBindInt64 stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap SqlRowId . sqlColumnInt64 stmt
    {-# INLINE fieldPeek #-}
