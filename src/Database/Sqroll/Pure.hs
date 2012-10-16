{-# LANGUAGE GADTs #-}
module Database.Sqroll.Pure
    where

import Control.Applicative ((<$>))
import Control.Monad (foldM)

import Database.Sqroll
import Database.Sqroll.Sqlite3

-- | Describes logging in a pure way
data Write c a where
    -- | Simple insertion
    Write
        :: HasTable a => a -> Write c a

    -- | Insertion depending on a foreign key
    WriteKey
        :: HasTable b => b -> (Key b -> [Write c a]) -> Write c a

    -- | Insertion depending on cached foreign keys
    WriteKeyCache
        :: HasTable b
        => b
        -> (c -> Maybe (Key b))
        -> (Key b -> c -> c)
        -> (Key b -> [Write c a]) -> Write c a

-- | Execute this in a transaction
runWrite :: Sqroll -> c -> [Write c a] -> IO c
runWrite sqroll = foldM go
  where
    go c (Write x) = sqrollAppend sqroll x >> return c

    go c (WriteKey x f) = do
        sqrollAppend sqroll x
        key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)
        foldM go c (f key)

    go c (WriteKeyCache x look insert f) = case look c of
        Just key -> foldM go c (f key)
        Nothing  -> do
            sqrollAppend sqroll x
            key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)
            foldM go (insert key c) (f key)
