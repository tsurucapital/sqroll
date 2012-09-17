{-# LANGUAGE FlexibleInstances #-}
module Database.Sqroll.Table.Field where

import Database.Sqroll.Table.Sqlite3

class Field a where
    fieldType :: a -> String  -- Should work with 'undefined'
    fieldPoke :: SqlStmt -> Int -> a -> IO ()
    fieldPeek :: SqlStmt -> Int -> IO a

instance Field Int where
    fieldType = const "INTEGER"

    fieldPoke stmt n = sqlBindInt64 stmt n . fromIntegral
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap fromIntegral (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field String where
    fieldType = const "TEXT"

    fieldPoke = sqlBindString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnString
    {-# INLINE fieldPeek #-}

instance Field Double where
    fieldType = const "DOUBLE"

    fieldPoke = sqlBindDouble
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnDouble
    {-# INLINE fieldPeek #-}
