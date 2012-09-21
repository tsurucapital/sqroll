{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Sqroll where

import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Generics (Generic, Rep, from, to)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table
import Database.Sqroll.Table.Generic

class HasTable t where
    table :: NamedTable t

    default table :: (Generic t, GNamedTable (Rep t)) => NamedTable t
    table = gNamedTable to from

-- | Create tables and indexes (if not exist...), ensure we have the correct
-- defaults
prepareTable :: HasTable a
             => Sql -> Maybe a -> IO (NamedTable a)
prepareTable sql defaultRecord = do
    sqlExecute sql $ tableCreate table'
    mapM_ (sqlExecute sql) $ tableIndexes table'
    tableMakeDefaults sql defaultRecord table'
  where
    table' = table

makeAppend :: forall a. (HasTable a)
           => Sql -> Maybe a -> IO (a -> IO (), IO ())
makeAppend sql defaultRecord = do
    table' <- prepareTable sql defaultRecord
    stmt   <- sqlPrepare sql $ tableInsert table'
    let poker = tablePoke table' stmt

    -- This should be reasonably fast
    let insert x = poker x >> sqlStep stmt >> sqlReset stmt

    return (insert, sqlFinalize stmt)

transaction :: Sql -> IO a -> IO a
transaction sql f = do
    sqlExecute sql "BEGIN"
    x <- f
    sqlExecute sql "COMMIT"
    return x

makeTail :: HasTable a => Sql -> Maybe a -> (a -> IO ()) -> IO (IO (), IO ())
makeTail sql defaultRecord f = do
    ref    <- newIORef (SqlRowId (-1))
    table' <- prepareTable sql defaultRecord
    stmt   <- sqlPrepare sql $ tableSelect table' ++ " WHERE rowid > ?"
    let peeker = tablePeek table' stmt

    let consume rowid = do
            moreData <- sqlStep stmt
            if moreData
                then do
                    x      <- peeker
                    rowid' <- sqlLastSelectRowid stmt
                    f x
                    consume rowid'
                else
                    return rowid
    
        tail' = do
            rowid <- readIORef ref
            sqlBindInt64 stmt 1 (unSqlRowId rowid)
            rowid' <- consume rowid
            writeIORef ref rowid'
            sqlReset stmt

    return (tail', sqlFinalize stmt)
