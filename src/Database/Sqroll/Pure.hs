{-# LANGUAGE GADTs #-}
module Database.Sqroll.Pure
    ( Log (..)
    , runLog
    ) where

import Data.IORef (IORef, readIORef, modifyIORef)

import Database.Sqroll

-- | Describes logging in a pure way.
--
-- * Log: Simple insertion
--
-- * LogKey: Insertion depending on a foreign key
--
-- * LogKeyCache: Insertion depending on cached foreign keys
--
data Log c where
    Log :: HasTable a => a -> Log c

    LogKey
        :: HasTable b => b -> (Key b -> [Log c]) -> Log c

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
    go (Log x) = sqrollAppend_ sqroll x

    go (LogKey x f) = do
        key <- sqrollAppend sqroll x
        mapM_ go (f key)

    go (LogKeyCache x look insert f) = do
        c <- readIORef cache
        case look c of
            Just key -> mapM_ go (f key)
            Nothing  -> do
                key <- sqrollAppend sqroll x
                modifyIORef cache (insert key)
                mapM_ go (f key)
