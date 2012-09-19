{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Sqroll where

import Control.Monad.Trans (MonadIO, liftIO)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table

class HasTable t where
    table :: NamedTable t

makeSqroll :: forall a. (HasTable a)
           => Sql -> IO (a -> IO (), IO ())
makeSqroll sql = do
    -- Create tables and indexes (if not exist...)
    sqlExecute sql $ tableCreate table'
    mapM_ (sqlExecute sql) $ tableIndexes table'

    -- Prepare an insert statement and a function to bind args
    stmt <- sqlPrepare sql $ tableInsert table'
    let poker = tablePoke table' stmt

    -- This should be reasonably fast
    let insert x = do
            poker x
            sqlStep stmt >> sqlReset stmt

    return (insert, sqlFinalize stmt)
  where
    table' :: NamedTable a
    table' = table
