{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Database.Sqroll.Internal
    ( NamedTable (..)

    , HasTable (..)
    , aliasTable

    , Key (..)
    , Entity (..)
    , Stmt (..)

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
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, modifyMVar, modifyMVar_, swapMVar)
import Control.Monad (unless, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import GHC.Generics (Generic, Rep, from, to)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak (addFinalizer)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table
import Database.Sqroll.Table.Field
import Database.Sqroll.Table.Generic

class HasTable t where
    table :: NamedTable t

    default table :: (Generic t, GNamedTable (Rep t)) => NamedTable t
    table = gNamedTable to from


instance (HasTable a, HasTable b) => HasTable (Key a, b) where
    table = let (NamedTable name_a _)   = table :: NamedTable a
                (NamedTable name_b tbl) = table :: NamedTable b
                name = name_b ++ "__of__" ++ name_a
                key = Primitive $ FieldInfo (name_a ++ "__id") fst

            in NamedTable name ((,) <$> key <*> rebuild tbl)
                where
                    rebuild :: Table b x -> Table (Key a, b) x
                    rebuild (Primitive (FieldInfo n a)) = Primitive (FieldInfo n (a . snd))
                    rebuild (Map f t) = Map f (rebuild t)
                    rebuild (Pure a) = Pure a
                    rebuild (App f t) = App (rebuild f) (rebuild t)

instance HasTable a => Field [a] where
    fieldTypes   = const []
    fieldDefault = []
    fieldPoke _ _ _ = return ()
    fieldPeek _ _ = return []

-- | Useful for creating tables for newtypes
aliasTable :: HasTable t => String -> (t -> u) -> (u -> t) -> NamedTable u
aliasTable name mk unmk =
    let NamedTable _ table' = table
    in NamedTable name $ mapTable mk unmk table'

-- Maybe rename to SqrollKey?
newtype Key a = Key {unKey :: SqlRowId}
    deriving (Eq, Show, Enum, Ord)

-- | Sql statement with insertion support
newtype IStmt a = IStmt (SqlStmt, SqlStmt -> a -> IO ())

-- | Sql statement with peek support
newtype Stmt a = Stmt { unStmt :: (SqlStmt, SqlStmt -> IO (Maybe a), Sqroll) }


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

-- | Make a statement to select every item of the given type
--
-- You can pass a default value - its fields will be used to replace those
-- missing from the database. If Nothing is passed instead missing fields will
-- be derived automatically - most likely empty strings, 0 values and so on.
makeSelectStatement :: HasTable a => Sqroll -> Maybe a -> IO (Stmt a)
makeSelectStatement sqroll defaultRecord = do
    table' <- prepareTable sqroll defaultRecord
    stmt   <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? ORDER BY rowid")
    sqlBindInt64 stmt 1 0
    sqrollRegisterStmt (stmt, mkSelectPeek table', sqroll)


-- | Make a statement to select every item of the given type taking
-- only those where foreign key value matches to a given one.
makeSelectByKeyStatement :: forall a b. (HasTable a, HasTable b)
            => Sqroll -> Maybe a -> Key b -> IO (Stmt a)
makeSelectByKeyStatement sqroll defaultRecord key = do
    table' <- prepareTable sqroll defaultRecord
    case tableRefers foreignTable table' of
        [c] -> do
            stmt <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? AND " ++ c ++ " = ? ORDER BY rowid")
            sqlBindInt64 stmt 1 0
            sqlBindInt64 stmt 2 (unKey key)
            sqrollRegisterStmt (stmt, mkSelectPeek table', sqroll)
        [] -> error' $ "Table " ++ tableName table' ++
            " does not refer to Table " ++ tableName foreignTable
        _ -> error' $ "There is more than one reference from " ++ tableName table' ++
                    " to " ++ tableName foreignTable ++ " so I don't know which one to use."
  where
    error'       = error . ("Database.Sqroll.Internal.makeSelectByKeyStatement: " ++)
    foreignTable = table :: NamedTable b

-- | By default select statements return raw values.
-- Use this to get Entires instead.
sqrollSelectEntitiy :: HasTable a => Stmt a -> Stmt (Entity a)
sqrollSelectEntitiy (Stmt (stmt, peek, sqroll)) = -- {{{
    let peek' s = do mVal <- peek s
                     case mVal of
                        Just entityVal -> do
                            entityKey <- Key <$> sqlGetRowId s
                            return $ Just Entity {..}
                        Nothing -> return Nothing
    in (Stmt (stmt, peek', sqroll))-- }}}

-- | Start from given rowid other than the first one
sqrollSelectFromRowId :: Stmt a -> Int64 -> IO ()
sqrollSelectFromRowId (Stmt (stmt, _, _)) = sqlBindInt64 stmt 1

-- | Get all available results from given statement as a one strict list
sqrollGetList :: Stmt a -> IO [a]
sqrollGetList (Stmt (stmt, peek, _)) = go-- {{{
    where
        go = do
            mPeekResult <- peek stmt
            case mPeekResult of
                Just v -> do
                    rest <- go
                    return $ v : rest
                Nothing -> return []-- }}}

-- | Get all available results from given statement as a one lazy list
sqrollGetLazyList :: Stmt a -> IO [a]
sqrollGetLazyList (Stmt (stmt, peek, _)) = go-- {{{
    where
        go = do
            mPeekResult <- peek stmt
            case mPeekResult of
                Just v -> do
                    rest <- unsafeInterleaveIO go
                    return $ v : rest
                Nothing -> return []-- }}}

-- | Fold over all available results in given statement
sqrollFoldAll :: (b -> a -> IO b) -> b -> Stmt a -> IO b
sqrollFoldAll f initialValue (Stmt (stmt, peek, _)) = go initialValue-- {{{
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
sqrollFold :: (b -> a -> IO (b, Bool)) -> b -> Stmt a -> IO b
sqrollFold f initialValue (Stmt (stmt, peek, _)) = go initialValue-- {{{
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
sqrollGetOne :: Stmt a -> IO a
sqrollGetOne (Stmt (stmt, peek, _)) = do-- {{{
    mPeekResult <- peek stmt
    case mPeekResult of
        Just a -> return a
        Nothing -> error "Expected to get at least one value in sqrollGetOne, but got none"-- }}}

-- | Finalize statement. If not finalized manually statement will
-- be finalized by GC
sqrollFinalize :: Stmt a -> IO ()
sqrollFinalize (Stmt (stmt, _, sqroll)) = do
        needs <- modifyMVar (sqrollStmts sqroll) needsFinalization
        when needs $ sqlFinalize stmt
    where
        needsFinalization :: [SqlStmt] -> IO ([SqlStmt], Bool)
        needsFinalization stmts = return (filter (/= stmt) stmts, elem stmt stmts)

sqrollRegisterStmt :: (SqlStmt, SqlStmt -> IO (Maybe a), Sqroll) -> IO (Stmt a)
sqrollRegisterStmt s@(stmt, _, sqroll) = do
    let s' = Stmt s
    modifyMVar_ (sqrollStmts sqroll) (return . (stmt :))
    addFinalizer s' (sqrollFinalize s')
    return s'



data Sqroll = Sqroll
    { sqrollSql        :: Sql
    , sqrollOpenFlags  :: [SqlOpenFlag]
    , sqrollLock       :: MVar ()
    , sqrollCache      :: IORef (HashMap String (SqrollCache ()))
    , sqrollStmts      :: MVar [SqlStmt]
    }

-- | Open sqroll log with sefault settings
sqrollOpen :: FilePath -> IO Sqroll
sqrollOpen filePath = sqrollOpenWith filePath sqlDefaultOpenFlags

-- | Open sqroll log with custom settings
sqrollOpenWith :: FilePath -> [SqlOpenFlag] -> IO Sqroll
sqrollOpenWith filePath flags = do
    s <- Sqroll <$> sqlOpen filePath flags <*> pure flags <*> newMVar ()
                <*> newIORef HM.empty <*> newMVar []
    addFinalizer s (sqrollClose s)
    return s

-- | Close sqroll log. All running statements will be finalized automatically
sqrollClose :: Sqroll -> IO ()
sqrollClose sqroll = do
        appenders <- HM.elems <$> readIORef (sqrollCache sqroll)
        mapM_ closeAppend appenders
        stmts <- swapMVar (sqrollStmts sqroll) []
        mapM_ sqlFinalize stmts
        sqlClose $ sqrollSql sqroll
    where
        closeAppend cache = do
            (IStmt (stmt, _)) <- takeMVar (sqrollCacheInsert cache)
            sqlFinalize stmt

-- | Move all the data from WAL file to the main db file. Checkpoint
-- will be performed automatically when database closed.
-- Doing checkpoints manually might make logging performance more predictable.
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

-- | Perform set of logging actions inside sqlite transaction
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
    _ <- sqlStep stmt
    sqlReset stmt
    putMVar (sqrollCacheInsert cache) cc
{-# INLINE sqrollAppend_ #-}
