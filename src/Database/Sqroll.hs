{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Sqroll where

import GHC.Generics (Generic, Rep, from, to)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table
import Database.Sqroll.Table.Generic

class HasTable t where
    table :: NamedTable t

    default table :: (Generic t, GNamedTable (Rep t)) => NamedTable t
    table = gNamedTable to from

makeSqroll :: forall a. (HasTable a)
           => Sql -> Maybe a -> IO (a -> IO (), IO ())
makeSqroll sql defaultRecord = do
    -- Create tables and indexes (if not exist...)
    sqlExecute sql $ tableCreate table'
    mapM_ (sqlExecute sql) $ tableIndexes table'

    -- Ensure we have the correct defaults
    table'' <- tableMakeDefaults sql defaultRecord table'

    -- Prepare an insert statement and a function to bind args
    stmt <- sqlPrepare sql $ tableInsert table''
    let poker = tablePoke table'' stmt

    -- This should be reasonably fast
    let insert x = poker x >> sqlStep stmt >> sqlReset stmt

    return (insert, sqlFinalize stmt)
  where
    table' :: NamedTable a
    table' = table
