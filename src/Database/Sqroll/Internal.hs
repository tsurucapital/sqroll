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
    , sqrollOpenReadOnly
    , sqrollOpenWith
    , sqrollClose
    , withSqroll
    , withSqrollReadOnly
    , withSqrollWith
    , sqrollCheckpoint

    , sqrollTransaction

    , sqrollAppend
    , sqrollAppend_

    , makeSelectStatement
    , makeSelectByKeyStatement

    , sqrollSelectEntity
    , sqrollSelectFromRowId
    , sqrollRebindKey

    , sqrollGetList
    , sqrollGetLazyList
    , sqrollFold
    , sqrollFoldAll
    , sqrollGetOne
    , sqrollGetMaybe

    , prepareTable
    , mkSelectPeek
    , sqrollInitializeTable
    ) where

import Control.Applicative (pure, (<$>), (<*>), (<$))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception.Lifted (bracket)
import Control.Monad (unless)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Foreign.ForeignPtr
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

-- Foreign key
newtype Key a = Key {unKey :: SqlRowId}
    deriving (Eq, Show, Enum, Ord)

-- | Sql statement with insertion support
newtype IStmt a = IStmt (SqlFStmt, SqlStmt -> a -> IO ())

-- | Sql statement with peek support, first argument shows result type
-- which you can get from this Stmt, second one shows if this statement operates with
-- specific foreign key
newtype Stmt a b = Stmt { unStmt :: (SqlFStmt, SqlStmt -> IO (Maybe a)) }


instance Show (Stmt a b) where
    show _ = "Sqroll Stmt"


data Entity a
    = Entity
    { entityKey :: (Key a)
    , entityVal :: a
    } deriving (Eq, Show, Ord)

instance forall a. HasTable a => Field (Key a) where
    fieldTypes   = const [SqlInteger]
    fieldIndexes = const [IndexFK $ tableName (table :: NamedTable a)]
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
        if hasData
            then Just <$> tablePeek table' stmt
            else do sqlReset stmt
                    return Nothing

-- | Make a statement to select every item of the given type
--
-- You can pass a default value - its fields will be used to replace those
-- missing from the database. If Nothing is passed instead missing fields will
-- be derived automatically - most likely empty strings, 0 values and so on.
makeSelectStatement :: HasTable a => Sqroll -> Maybe a -> IO (Stmt a a)
makeSelectStatement sqroll defaultRecord = do
    table' <- prepareTable sqroll defaultRecord
    stmt   <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? ORDER BY rowid")
    withForeignPtr stmt $ \raw -> sqlBindInt64 raw 1 0
    return $ Stmt (stmt, mkSelectPeek table')


-- | Make a statement to select every item of the given type taking
-- only those where foreign key value matches to a given one.
makeSelectByKeyStatement :: forall a b. (HasTable a, HasTable b)
            => Sqroll -> Maybe a -> Key b -> IO (Stmt a (Key b))
makeSelectByKeyStatement sqroll defaultRecord key = do
    table' <- prepareTable sqroll defaultRecord
    case tableRefers foreignTable table' of
        [c] -> do
            stmt <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? AND " ++ c ++ " = ? ORDER BY rowid")
            withForeignPtr stmt $ \raw -> do
                    sqlBindInt64 raw 1 0
                    sqlBindInt64 raw 2 (unKey key)
            return $ Stmt (stmt, mkSelectPeek table')
        [] -> error' $ "Table " ++ tableName table' ++
            " does not refer to Table " ++ tableName foreignTable
        _ -> error' $ "There is more than one reference from " ++ tableName table' ++
                    " to " ++ tableName foreignTable ++ " so I don't know which one to use."
  where
    error'       = error . ("Database.Sqroll.Internal.makeSelectByKeyStatement: " ++)
    foreignTable = table :: NamedTable b

-- | By default select statements return raw values.
-- Use this to get Entires instead.
sqrollSelectEntity :: HasTable a => Stmt a b -> Stmt (Entity a) b
sqrollSelectEntity (Stmt (stmt, peek)) = -- {{{
    let peek' s = do mVal <- peek s
                     case mVal of
                        Just entityVal -> do
                            entityKey <- Key <$> sqlGetRowId s
                            return $ Just Entity {..}
                        Nothing -> return Nothing
    in (Stmt (stmt, peek'))-- }}}

-- | Start from given rowid other than the first one
sqrollSelectFromRowId :: Stmt a b -> Int64 -> IO ()
sqrollSelectFromRowId (Stmt (stmt, _)) i = withForeignPtr stmt $ \raw -> sqlBindInt64 raw 1 i

-- | Bind a new value for the foreign key specified in this statement
sqrollRebindKey :: HasTable b => Stmt a (Key b) -> Int64 -> IO ()
sqrollRebindKey (Stmt (stmt, _)) i = withForeignPtr stmt $ \raw -> sqlBindInt64 raw 2 i

-- | Get all available results from given statement as a one strict list
sqrollGetList :: Stmt a b -> IO [a]
sqrollGetList (Stmt (stmt, peek)) = go-- {{{
    where
        go = withForeignPtr stmt $ \raw -> do
            mPeekResult <- peek raw
            case mPeekResult of
                Just v -> do
                    rest <- go
                    return $ v : rest
                Nothing -> sqlReset raw >> return []-- }}}

-- | Get all available results from given statement as a one lazy list
sqrollGetLazyList :: Stmt a b -> IO [a]
sqrollGetLazyList (Stmt (stmt, peek)) = go-- {{{
    where
        go = withForeignPtr stmt $ \raw -> do
            mPeekResult <- peek raw
            case mPeekResult of
                Just v -> do
                    rest <- unsafeInterleaveIO go
                    return $ v : rest
                Nothing -> sqlReset raw >> return []-- }}}

-- | Fold over all available results in given statement
sqrollFoldAll :: MonadIO m => (b -> a -> m b) -> b -> Stmt a c -> m b
sqrollFoldAll f initialValue (Stmt (stmt, peek)) = go initialValue-- {{{
    where
        go b = do
            mPeekResult <- liftIO $ withForeignPtr stmt peek
            case mPeekResult of
                Just a -> do
                    b' <- f b a
                    go b'
                Nothing -> liftIO (withForeignPtr stmt sqlReset) >> return b-- }}}

-- | Fold over all available results in given statement with option to interrupt computation
-- (return False as second element of the pair to interrupt), after interruption statement
-- will point to the next available row
sqrollFold :: MonadIO m => (b -> a -> m (b, Bool)) -> b -> Stmt a c -> m b
sqrollFold f initialValue fstmt@(Stmt (stmt, peek)) = go initialValue-- {{{
    where
        go b = do
            mPeekResult <- liftIO $ withForeignPtr stmt peek
            case mPeekResult of
                Just a -> do
                    (b', continueFolding) <- f b a
                    if continueFolding
                        then go b'
                        else liftIO $ withForeignPtr stmt $ \raw -> do
                                rowId <- sqlGetRowId raw
                                sqlReset raw
                                sqrollSelectFromRowId fstmt rowId
                                return b'
                Nothing -> liftIO (withForeignPtr stmt sqlReset) >> return b-- }}}

-- | Get one value from the statement, will die with error in case of failure
sqrollGetOne :: Stmt a b -> IO a
sqrollGetOne (Stmt (stmt, peek)) = withForeignPtr stmt $ \raw -> do-- {{{
    mPeekResult <- peek raw
    case mPeekResult of
        Just a -> sqlReset raw >> return a
        Nothing -> error "Expected to get at least one value in sqrollGetOne, but got none"-- }}}

-- | Get one value if it's available
sqrollGetMaybe :: Stmt a b -> IO (Maybe a)
sqrollGetMaybe (Stmt (stmt, peek)) = withForeignPtr stmt $ \raw -> do
    result <- peek raw
    sqlReset raw
    return result


data Sqroll = Sqroll
    { sqrollSql        :: Sql
    , sqrollOpenFlags  :: [SqlOpenFlag]
    , sqrollLock       :: MVar ()
    , sqrollCache      :: IORef (HashMap String (SqrollCache ()))
    }

-- | Open sqroll log with sefault settings.
--
-- For most cases, it is recommended to use 'withSqroll' instead,
-- which guarantees properly closing the database, even in the
-- presence of exceptions.
--
-- If you do not need to write to the database, prefer 'sqrollOpenReadOnly'
-- / 'withSqrollReadOnly' to prevent SQLite from creating @-shm@ and @-wal@
-- files.
sqrollOpen :: FilePath -> IO Sqroll
sqrollOpen filePath = sqrollOpenWith filePath sqlDefaultOpenFlags

-- | Same as @'sqrollOpen' filePath ['SqlOpenReadOnly']@. See 'sqrollOpen' for benefits.
sqrollOpenReadOnly :: FilePath -> IO Sqroll
sqrollOpenReadOnly filePath = sqrollOpenWith filePath [SqlOpenReadOnly]

-- | @withSqroll path act@ opens a sqroll database using 'sqrollOpen' and passes
-- the resulting handle to the computation @act@.  The handle will be
-- closed on exit from 'withSqroll', whether by normal termination or by
-- raising an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'withSqroll' rather than any exception
-- raised by 'act'.
withSqroll :: (MonadBaseControl IO m) => FilePath -> (Sqroll -> m a) -> m a
withSqroll path = withSqrollWith path sqlDefaultOpenFlags

-- | Same as @'withSqrollReadOnly' filePath ['SqlOpenReadOnly'] action@. See 'sqrollOpen' for benefits.
withSqrollReadOnly :: (MonadBaseControl IO m) => FilePath -> (Sqroll -> m a) -> m a
withSqrollReadOnly path = withSqrollWith path [SqlOpenReadOnly]

-- | Same as withSqroll with custom settings.
withSqrollWith :: (MonadBaseControl IO m) => FilePath -> [SqlOpenFlag] -> (Sqroll -> m a) -> m a
withSqrollWith flags filename = bracket (liftBase $ sqrollOpenWith flags filename) (liftBase . sqrollClose)

-- | Open sqroll log with custom settings
sqrollOpenWith :: FilePath -> [SqlOpenFlag] -> IO Sqroll
sqrollOpenWith filePath flags = do
    s <- Sqroll <$> sqlOpen filePath flags <*> pure flags <*> newMVar ()
                <*> newIORef HM.empty
    return s

-- | Close sqroll log. All running statements will be finalized automatically
sqrollClose :: Sqroll -> IO ()
sqrollClose = sqlClose . sqrollSql

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

sqrollInitializeTable :: forall a. HasTable a => Sqroll -> a -> IO ()
sqrollInitializeTable sqroll _ = () <$ (sqrollGetCache sqroll :: IO (SqrollCache a))

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
    rowId <- withForeignPtr stmt $ \raw -> do
                    sqlStep_ raw
                    sqlColumnInt64 raw 0
    sqlExecute (sqrollSql sqroll) "RELEASE SAVEPOINT getrowid"
    return (Key rowId)
{-# INLINE sqrollAppend #-}

-- | Appends a new value to sqroll db, cheaper than 'sqrollAppend'
sqrollAppend_ :: HasTable a => Sqroll -> a -> IO ()
sqrollAppend_ sqroll x = do
    cache <- sqrollGetCache sqroll
    cc@(IStmt (stmt, poker)) <- takeMVar (sqrollCacheInsert cache)
    withForeignPtr stmt $ \raw -> do
             poker raw x
             sqlStep_ raw
             sqlReset raw
    putMVar (sqrollCacheInsert cache) cc
{-# INLINE sqrollAppend_ #-}
