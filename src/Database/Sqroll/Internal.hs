{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Sqroll.Internal
    ( NamedTable (..)

    , HasTable (..)
    , aliasTable

    , Key (..)

    , Sqroll (sqrollSql)
    , sqrollOpen
    , sqrollOpenWith
    , sqrollClose
    , sqrollTransaction
    , sqrollAppend
    , sqrollTail
    , sqrollTailList
    , sqrollSelect
    , sqrollByKey
    , sqrollAllByKey
    , sqrollSetDefault
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import GHC.Generics (Generic, Rep, from, to)
import Unsafe.Coerce (unsafeCoerce)

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

instance forall a. HasTable a => Field (Key a) where
    fieldTypes   = const [SqlInteger]
    fieldRefers  = const [tableName (table :: NamedTable a)]
    fieldDefault = Key (-1)

    fieldPoke stmt n (Key x) = sqlBindInt64 stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap Key . sqlColumnInt64 stmt
    {-# INLINE fieldPeek #-}

data SqrollCache a = SqrollCache
    { sqrollCacheAppend   :: a -> IO ()
    , sqrollCacheTail     :: Key a -> (a -> IO ()) -> IO (Key a)
    , sqrollCacheSelect   :: Key a -> IO a
    , sqrollCacheFinalize :: IO ()
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
    table'     <- prepareTable sqroll defaultRecord
    appendStmt <- sqlPrepare sql $ tableInsert table'
    tailStmt   <- sqlPrepare sql $
        tableSelect table' ++ " WHERE rowid >= ? ORDER BY rowid"
    let poker  = tablePoke table' appendStmt
        peeker = tablePeek table' tailStmt

    -- These should be reasonably fast
    let append x = poker x >> sqlStep appendStmt >> sqlReset appendStmt

        tail' (Key rowid) f = do
            sqlBindInt64 tailStmt 1 rowid
            sqlStepAll tailStmt (peeker >>= f)
            rowid' <- sqlGetRowId tailStmt
            sqlReset tailStmt
            return $ Key $ rowid' + 1

        select (Key rowid) = do
            sqlBindInt64 tailStmt 1 rowid
            sqlStep_ tailStmt
            x <- peeker
            sqlReset tailStmt
            return x

    return SqrollCache
        { sqrollCacheAppend   = append
        , sqrollCacheTail     = tail'
        , sqrollCacheSelect   = select
        , sqrollCacheFinalize = do
            sqlFinalize appendStmt
            sqlFinalize tailStmt
        }
  where
    sql = sqrollSql sqroll

data Sqroll = Sqroll
    { sqrollSql        :: Sql
    , sqrollOpenFlags  :: [SqlOpenFlag]
    , sqrollLock       :: MVar ()
    , sqrollCache      :: IORef (HashMap String (SqrollCache ()))
    , sqrollFinalizers :: IORef [IO ()]
    }

sqrollOpen :: FilePath -> IO Sqroll
sqrollOpen filePath = sqrollOpenWith filePath sqlDefaultOpenFlags

sqrollOpenWith :: FilePath -> [SqlOpenFlag] -> IO Sqroll
sqrollOpenWith filePath flags = Sqroll
    <$> sqlOpen filePath flags <*> pure flags <*> newMVar ()
    <*> newIORef HM.empty <*> newIORef []

sqrollClose :: Sqroll -> IO ()
sqrollClose sqroll = do
    sequence_ =<< readIORef (sqrollFinalizers sqroll)
    sequence_ . map sqrollCacheFinalize . HM.elems =<<
        readIORef (sqrollCache sqroll)
    sqlClose $ sqrollSql sqroll

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

sqrollAppend :: HasTable a => Sqroll -> a -> IO ()
sqrollAppend sqroll x = do
    cache <- sqrollGetCache sqroll
    sqrollCacheAppend cache x
{-# INLINE sqrollAppend #-}

sqrollTail :: HasTable a => Sqroll -> Key a -> (a -> IO ()) -> IO (Key a)
sqrollTail sqroll key f = do
    cache <- sqrollGetCache sqroll
    sqrollCacheTail cache key f
{-# INLINE sqrollTail #-}

sqrollTailList :: HasTable a => Sqroll -> Key a -> IO ([a], Key a)
sqrollTailList sqroll key = do
    ref  <- newIORef []
    key' <- sqrollTail sqroll key $ \x -> modifyIORef ref (x :)
    xs   <- reverse <$> readIORef ref
    return (xs, key')

sqrollSelect :: HasTable a => Sqroll -> Key a -> IO a
sqrollSelect sqroll key = do
    cache <- sqrollGetCache sqroll
    sqrollCacheSelect cache key
{-# INLINE sqrollSelect #-}


sqrollAllByKey :: (HasTable a, HasTable b)
            => Sqroll -> Maybe a -> Key b -> IO [a]
sqrollAllByKey db dflt key = do
    ref <- newIORef []
    sqrollByKey db dflt key (\x -> modifyIORef ref (x :))
    reverse <$> readIORef ref

sqrollByKey :: forall a b. (HasTable a, HasTable b)
            => Sqroll -> Maybe a -> Key b -> (a -> IO ()) -> IO ()
sqrollByKey sqroll defaultRecord key f = do
    -- Note: this method currently doesn't use caching of statements/peekers as
    -- the other methods in the module do.
    table' <- prepareTable sqroll defaultRecord
    let columns = tableRefers foreignTable table'

    case columns of
        [] -> error' $ "Table " ++ tableName table' ++
            " does not refer to Table " ++ tableName foreignTable
        (c : _) -> do
            stmt <- sqlPrepare sql $
                tableSelect table' ++ " WHERE " ++ c ++ " = ?"
            let peeker = tablePeek table' stmt
            sqlBindInt64 stmt 1 (unKey key)
            sqlStepAll stmt (peeker >>= f)
            sqlFinalize stmt
  where
    sql          = sqrollSql sqroll
    error'       = error . ("Database.Sqroll.Internal.sqrollByKey: " ++)
    foreignTable = table :: NamedTable b

sqrollSetDefault :: forall a. HasTable a => Sqroll -> Maybe a -> IO ()
sqrollSetDefault sqroll defaultRecord = do
    -- Run finalizers for previous default
    cache <- readIORef (sqrollCache sqroll)
    case HM.lookup name cache of
        Just sq -> sqrollCacheFinalize sq
        _       -> return ()

    -- Install new default
    sq <- makeSqrollCacheFor sqroll defaultRecord
    writeIORef (sqrollCache sqroll) $
        HM.insert name (unsafeCoerce sq) cache
  where
    table' = table :: NamedTable a
    name   = tableName table'
