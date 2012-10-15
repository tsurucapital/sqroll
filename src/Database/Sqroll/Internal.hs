{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Sqroll.Internal
    ( NamedTable (..)

    , HasTable (..)
    , aliasTable

    , SqlKey (..)

    , Sqroll (sqrollSql)
    , sqrollOpen
    , sqrollOpenWith
    , sqrollClose
    , sqrollTransaction
    , sqrollAppend
    , sqrollTail
    , sqrollSelect
    , sqrollByKey
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
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
newtype SqlKey a = SqlKey {unSqlKey :: SqlRowId}
    deriving (Eq, Show)

instance forall a. HasTable a => Field (SqlKey a) where
    fieldType    = const SqlInteger
    fieldRefers  = const [tableName (table :: NamedTable a)]
    fieldDefault = SqlKey (-1)

    fieldPoke stmt n (SqlKey x) = sqlBindInt64 stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap SqlKey . sqlColumnInt64 stmt
    {-# INLINE fieldPeek #-}

data SqrollCache a = SqrollCache
    { sqrollCacheAppend   :: a -> IO ()
    , sqrollCacheFinalize :: IO ()
    }

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

makeSqrollCacheFor :: HasTable a => Sql -> Maybe a -> IO (SqrollCache a)
makeSqrollCacheFor sql defaultRecord = do
    table'     <- prepareTable sql defaultRecord
    appendStmt <- sqlPrepare sql $ tableInsert table'
    let poker = tablePoke table' appendStmt

    -- This should be reasonably fast
    let append x = poker x >> sqlStep appendStmt >> sqlReset appendStmt

    return SqrollCache
        { sqrollCacheAppend   = append
        , sqrollCacheFinalize = sqlFinalize appendStmt
        }

data Sqroll = Sqroll
    { sqrollSql        :: Sql
    , sqrollLock       :: MVar ()
    , sqrollCache      :: IORef (HashMap String (SqrollCache ()))
    , sqrollFinalizers :: IORef [IO ()]
    }

sqrollOpen :: FilePath -> IO Sqroll
sqrollOpen filePath = sqrollOpenWith filePath sqlDefaultOpenFlags

sqrollOpenWith :: FilePath -> [SqlOpenFlag] -> IO Sqroll
sqrollOpenWith filePath flags = Sqroll
    <$> sqlOpen filePath flags <*> newMVar ()
    <*> newIORef HM.empty <*> newIORef []

sqrollClose :: Sqroll -> IO ()
sqrollClose sqroll = do
    sequence_ =<< readIORef (sqrollFinalizers sqroll)
    sequence_ . map sqrollCacheFinalize . HM.elems =<<
        readIORef (sqrollCache sqroll)
    sqlClose $ sqrollSql sqroll

sqrollGetCache :: forall a. HasTable a => Sqroll -> IO (SqrollCache a)
sqrollGetCache sqroll = do
    cache <- readIORef (sqrollCache sqroll)
    case HM.lookup name cache of
        Just sq -> return $ unsafeCoerce sq
        Nothing -> do
            sq <- makeSqrollCacheFor (sqrollSql sqroll) Nothing
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

sqrollTail :: HasTable a => Sqroll -> Maybe a -> (a -> IO ()) -> IO (IO ())
sqrollTail sqroll defaultRecord f = do
    (tail', finalizer) <- makeTail (sqrollSql sqroll) defaultRecord f
    modifyIORef (sqrollFinalizers sqroll) (finalizer :)
    return tail'

sqrollSelect :: HasTable a => Sqroll -> Maybe a -> IO (SqlKey a -> IO a)
sqrollSelect sqroll defaultRecord = do
    (select, finalizer) <- makeSelect (sqrollSql sqroll) defaultRecord
    modifyIORef (sqrollFinalizers sqroll) (finalizer :)
    return select

sqrollByKey :: forall a b. (HasTable a, HasTable b)
            => Sqroll -> Maybe a -> SqlKey b -> IO [a]
sqrollByKey sqroll defaultRecord key = do
    table' <- prepareTable sql defaultRecord
    let columns = tableRefers foreignTable table'

    case columns of
        [] -> error' $ "Table " ++ tableName table' ++
            " does not refer to Table " ++ tableName foreignTable
        (c : _) -> do
            stmt <- sqlPrepare sql $
                tableSelect table' ++ " WHERE " ++ c ++ " = ?"
            let peeker = tablePeek table' stmt
            sqlBindInt64 stmt 1 (unSqlKey key)
            xs <- sqlStepAll stmt peeker
            sqlFinalize stmt 
            return xs
  where
    sql          = sqrollSql sqroll
    error'       = error . ("Database.Sqroll.Internal.sqrollByKey: " ++)
    foreignTable = table :: NamedTable b

makeTail :: HasTable a => Sql -> Maybe a -> (a -> IO ()) -> IO (IO (), IO ())
makeTail sql defaultRecord f = do
    ref    <- newIORef 0
    table' <- prepareTable sql defaultRecord
    stmt   <- sqlPrepare sql $ tableSelect table' ++ " WHERE rowid >= ?"
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
            sqlBindInt64 stmt 1 rowid
            rowid' <- consume rowid
            writeIORef ref (rowid' + 1)
            sqlReset stmt

    return (tail', sqlFinalize stmt)

makeSelect :: HasTable a => Sql -> Maybe a -> IO (SqlKey a -> IO a, IO ())
makeSelect sql defaultRecord = do
    table' <- prepareTable sql defaultRecord
    stmt   <- sqlPrepare sql $ tableSelect table' ++ " WHERE rowid = ?"
    let peeker = tablePeek table' stmt

    let select rowid = do
            sqlBindInt64 stmt 1 (unSqlKey rowid)
            sqlStep_ stmt
            x <- peeker
            sqlReset stmt
            return x

    return (select, sqlFinalize stmt)
