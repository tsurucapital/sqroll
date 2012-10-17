{-# LANGUAGE GADTs #-}
module Database.Sqroll.Pure
    ( Log (..)
    , runLog
    ) where

import Control.Applicative ((<$>))
import Data.IORef (IORef, readIORef, modifyIORef)

import Database.Sqroll
import Database.Sqroll.Sqlite3

-- | Describes logging in a pure way
data Log c where
    -- | Simple insertion
    Log :: HasTable a => a -> Log c

    -- | Insertion depending on a foreign key
    LogKey
        :: HasTable b => b -> (Key b -> [Log c]) -> Log c

    -- | Insertion depending on cached foreign keys
    LogKeyCache
        :: HasTable b
        => b
        -> (c -> Maybe (Key b))
        -> (Key b -> c -> c)
        -> (Key b -> [Log c]) -> Log c

-- | Execute this in a transaction
runLog :: Sqroll -> IORef c -> [Log c] -> IO ()
runLog sqroll cache = mapM_ go
  where
    go (Log x) = sqrollAppend sqroll x

    go (LogKey x f) = do
        sqrollAppend sqroll x
        key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)
        mapM_ go (f key)

    go (LogKeyCache x look insert f) = do
        c <- readIORef cache
        case look c of
            Just key -> mapM_ go (f key)
            Nothing  -> do
                sqrollAppend sqroll x
                key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)
                modifyIORef cache (insert key)
                mapM_ go (f key)
