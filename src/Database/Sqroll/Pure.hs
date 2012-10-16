{-# LANGUAGE GADTs #-}
module Database.Sqroll.Pure
    ( Log (..)
    , runLog
    ) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)

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
runLog :: Sqroll -> c -> [Log c] -> IO c
runLog sqroll = foldM go
  where
    go c (Log x) = sqrollAppend sqroll x >> return c

    go c (LogKey x f) = do
        sqrollAppend sqroll x
        key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)
        foldM go c (f key)

    go c (LogKeyCache x look insert f) = case look c of
        Just key -> foldM go c (f key)
        Nothing  -> do
            sqrollAppend sqroll x
            key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)
            foldM go (insert key c) (f key)
