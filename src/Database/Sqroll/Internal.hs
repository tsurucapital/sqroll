{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Sqroll.Internal
    ( NamedTable (..)

    , HasTable (..)
    , aliasTable

    , Key (..)
    , Entity (..)

    , Sqroll (sqrollSql)
    , sqrollOpen
    , sqrollOpenWith
    , sqrollClose
    , sqrollCheckpoint
    , sqrollFinalize

    , sqrollTransaction

    , sqrollAppend
    , sqrollAppend_

    , makeSelectStatement
    , makeSelectByKeyStatement

    , sqrollSelectEntitiy
    , sqrollSelectFromRowId

    , sqrollGetList
    , sqrollGetLazyList
    , sqrollFold
    , sqrollFoldAll
    , sqrollGetOne
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import GHC.Generics (Generic, Rep, from, to)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafeInterleaveIO)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table
import Database.Sqroll.Table.Field
import Database.Sqroll.Table.Generic

class HasTable t where
    table :: NamedTable t

    default table :: (Generic t, GNamedTable (Rep t)) => NamedTable t
    table = gNamedTable to from

-- | Useful for creating tables for newtypes
aliasTable :: HasTable t => String -> (t -> u) -> (u -> t) -> NamedTable u
aliasTable name mk unmk =
    let NamedTable _ table' = table
    in NamedTable name $ mapTable mk unmk table'

-- Maybe rename to SqrollKey?
newtype Key a = Key {unKey :: SqlRowId}
    deriving (Eq, Show, Enum, Ord)

-- | Sql statement with insertion support
newtype IStmt a = IStmt { unIStmt :: (SqlStmt, SqlStmt -> a -> IO ()) }

-- | Sql statement with peek support
newtype SStmt a = SStmt { unSStmt :: (SqlStmt, SqlStmt -> IO (Maybe a)) }


data Entity a
    = Entity
    { entityKey :: (Key a)
    , entityVal :: a
    } deriving (Eq, Show, Ord)

instance forall a. HasTable a => Field (Key a) where
    fieldTypes   = const [SqlInteger]
    fieldRefers  = const [tableName (table :: NamedTable a)]
    fieldDefault = Key (-1)

    fieldPoke stmt n (Key x) = sqlBindInt64 stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap Key . sqlColumnInt64 stmt
    {-# INLINE fieldPeek #-}

data SqrollCache a = SqrollCache
    { sqrollCacheInsert :: MVar (IStmt a)
    }

-- | Create tables and indexes (if not exist...), ensure we have the correct
-- defaults
prepareTable :: HasTable a
             => Sqroll -> Maybe a -> IO (NamedTable a)
prepareTable sqroll defaultRecord = do
    unless (sqrollReadOnly sqroll) $ do
        sqlExecute sql $ tableCreate table'
        mapM_ (sqlExecute sql) $ tableIndexes table'
    tableMakeDefaults sql defaultRecord table'
  where
    sql    = sqrollSql sqroll
    table' = table

makeSqrollCacheFor :: HasTable a => Sqroll -> Maybe a -> IO (SqrollCache a)
makeSqrollCacheFor sqroll defaultRecord = do
    table' <- prepareTable sqroll defaultRecord
    stmt   <- sqlPrepare (sqrollSql sqroll) (tableInsert table')
    cache  <- newMVar $ IStmt (stmt, tablePoke table')

    return SqrollCache
        { sqrollCacheInsert = cache
        }

mkSelectPeek :: NamedTable a -> SqlStmt -> IO (Maybe a)
mkSelectPeek table' stmt = do
        hasData <- sqlStep stmt
        r <- if hasData
            then Just <$> tablePeek table' stmt
            else do sqlReset stmt
                    return Nothing
        return r


makeSelectStatement :: HasTable a => Sqroll -> Maybe a -> IO (SStmt a)
makeSelectStatement sqroll defaultRecord = do
    table' <- prepareTable sqroll defaultRecord
    stmt   <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? ORDER BY rowid")
    sqlBindInt64 stmt 1 0
    return $ SStmt (stmt, mkSelectPeek table')



makeSelectByKeyStatement :: forall a b. (HasTable a, HasTable b)
            => Sqroll -> Maybe a -> Key b -> IO (SStmt a)
makeSelectByKeyStatement sqroll defaultRecord key = do
    table' <- prepareTable sqroll defaultRecord
    case tableRefers foreignTable table' of
        [c] -> do
            stmt <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? AND " ++ c ++ " = ? ORDER BY rowid")
            sqlBindInt64 stmt 1 0
            sqlBindInt64 stmt 2 (unKey key)
            return $ SStmt (stmt, mkSelectPeek table')
        [] -> error' $ "Table " ++ tableName table' ++
            " does not refer to Table " ++ tableName foreignTable
        _ -> error' $ "There is more than one reference from " ++ tableName table' ++
                    " to " ++ tableName foreignTable ++ " so I don't know which one to use."
  where
    error'       = error . ("Database.Sqroll.Internal.makeSelectByKeyStatement: " ++)
    foreignTable = table :: NamedTable b

-- | By default select statements return raw values.
-- Use this to get Entires instead.
sqrollSelectEntitiy :: HasTable a => SStmt a -> SStmt (Entity a)
sqrollSelectEntitiy (SStmt (stmt, peek)) = -- {{{
    let peek' s = do mVal <- peek s
                     case mVal of
                        Just entityVal -> do
                            entityKey <- Key <$> sqlGetRowId s
                            return $ Just Entity {..}
                        Nothing -> return Nothing
    in (SStmt (stmt, peek'))-- }}}

-- | Start from given rowid other than the first one
sqrollSelectFromRowId :: SStmt a -> Int64 -> IO ()
sqrollSelectFromRowId (SStmt (stmt, _)) = sqlBindInt64 stmt 1

-- | Get all available results from given statement as a one strict list
sqrollGetList :: SStmt a -> IO [a]
sqrollGetList (SStmt (stmt, peek)) = go-- {{{
    where
        go = do
            mPeekResult <- peek stmt
            case mPeekResult of
                Just v -> do
                    rest <- go
                    return $ v : rest
                Nothing -> return []-- }}}

-- | Get all available results from given statement as a one lazy list
sqrollGetLazyList :: SStmt a -> IO [a]
sqrollGetLazyList (SStmt (stmt, peek)) = go-- {{{
    where
        go = do
            mPeekResult <- peek stmt
            case mPeekResult of
                Just v -> do
                    rest <- unsafeInterleaveIO go
                    return $ v : rest
                Nothing -> return []-- }}}

-- | Fold over all available results in given statement
sqrollFoldAll :: (b -> a -> IO b) -> b -> SStmt a -> IO b
sqrollFoldAll f initialValue (SStmt (stmt, peek)) = go initialValue-- {{{
    where
        go b = do
            mPeekResult <- peek stmt
            case mPeekResult of
                Just a -> do
                    b' <- f b a
                    go b'
                Nothing -> return b-- }}}

-- | Fold over all available results in given statement with option to interrupt computation
-- (return False as second element of the pair to interrupt)
sqrollFold :: (b -> a -> IO (b, Bool)) -> b -> SStmt a -> IO b
sqrollFold f initialValue (SStmt (stmt, peek)) = go initialValue-- {{{
    where
        go b = do
            mPeekResult <- peek stmt
            case mPeekResult of
                Just a -> do
                    (b', continueFolding) <- f b a
                    if continueFolding
                        then go b'
                        else return b'
                Nothing -> return b-- }}}

-- | Get one value from the statement, will die with error in case of failure
sqrollGetOne :: SStmt a -> IO a
sqrollGetOne (SStmt (stmt, peek)) = do-- {{{
    mPeekResult <- peek stmt
    case mPeekResult of
        Just a -> return a
        Nothing -> error "Expected to get at least one value in sqrollGetOne, but got none"-- }}}

-- | Finalize statement. All statements must be finalized, do not use it.
sqrollFinalize :: SStmt a -> IO ()
sqrollFinalize (SStmt (stmt, _)) = sqlFinalize stmt


data Sqroll = Sqroll
    { sqrollSql        :: Sql
    , sqrollOpenFlags  :: [SqlOpenFlag]
    , sqrollLock       :: MVar ()
    , sqrollCache      :: IORef (HashMap String (SqrollCache ()))
    }

sqrollOpen :: FilePath -> IO Sqroll
sqrollOpen filePath = sqrollOpenWith filePath sqlDefaultOpenFlags

sqrollOpenWith :: FilePath -> [SqlOpenFlag] -> IO Sqroll
sqrollOpenWith filePath flags = Sqroll
    <$> sqlOpen filePath flags <*> pure flags <*> newMVar ()
    <*> newIORef HM.empty

sqrollClose :: Sqroll -> IO ()
sqrollClose sqroll = do
        appenders <- HM.elems <$> readIORef (sqrollCache sqroll)
        mapM_ closeAppend appenders
        sqlClose $ sqrollSql sqroll
    where
        closeAppend cache = do
            (IStmt (stmt, _)) <- takeMVar (sqrollCacheInsert cache)
            sqlFinalize stmt

sqrollCheckpoint :: Sqroll -> IO ()
sqrollCheckpoint = sqlCheckpoint . sqrollSql

sqrollReadOnly :: Sqroll -> Bool
sqrollReadOnly = (SqlOpenReadOnly `elem`) . sqrollOpenFlags

sqrollGetCache :: forall a. HasTable a => Sqroll -> IO (SqrollCache a)
sqrollGetCache sqroll = do
    cache <- readIORef (sqrollCache sqroll)
    case HM.lookup name cache of
        Just sq -> return $ unsafeCoerce sq
        Nothing -> do
            sq <- makeSqrollCacheFor sqroll Nothing
            writeIORef (sqrollCache sqroll) $
                HM.insert name (unsafeCoerce sq) cache
            return sq
  where
    table' = table :: NamedTable a
    name   = tableName table'

sqrollTransaction :: MonadIO m => Sqroll -> m a -> m a
sqrollTransaction sqroll f = do
    () <- liftIO $ takeMVar (sqrollLock sqroll)
    liftIO $ sqlExecute (sqrollSql sqroll) "BEGIN"
    x <- f
    liftIO $ sqlExecute (sqrollSql sqroll) "COMMIT"
    liftIO $ putMVar (sqrollLock sqroll) ()
    return x

-- | Append value to sqroll db, retrive a key for that value. In most cases
-- you want to use 'sqrollAppend_'
sqrollAppend :: HasTable a => Sqroll -> a -> IO (Key a)
sqrollAppend sqroll x = do
    sqlExecute (sqrollSql sqroll) "SAVEPOINT getrowid"
    sqrollAppend_ sqroll x
    stmt <- sqlPrepare (sqrollSql sqroll) "SELECT last_insert_rowid();"
    sqlStep_ stmt
    rowId <- sqlColumnInt64 stmt 0
    sqlFinalize stmt
    sqlExecute (sqrollSql sqroll) "RELEASE SAVEPOINT getrowid"
    return (Key rowId)
{-# INLINE sqrollAppend #-}

-- | Appends a new value to sqroll db, cheaper than 'sqrollAppend'
sqrollAppend_ :: HasTable a => Sqroll -> a -> IO ()
sqrollAppend_ sqroll x = do
    cache <- sqrollGetCache sqroll
    cc@(IStmt (stmt, poker)) <- takeMVar (sqrollCacheInsert cache)
    poker stmt x
    sqlStep stmt
    sqlReset stmt
    putMVar (sqrollCacheInsert cache) cc
{-# INLINE sqrollAppend_ #-}
