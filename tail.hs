{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import GHC.Generics (Generic)

import Database.Sqroll
import Database.Sqroll.Table
import Database.Sqroll.Sqlite3

data Person = Person
    { personName :: String
    , personAge  :: Int
    } deriving (Generic, Show)

instance HasTable Person

main :: IO ()
main = do
    sql               <- sqlOpen "live.db"
    (tail', finalize) <- makeTail sql (Nothing :: Maybe Person) print

    forever $ do
        tail'
        threadDelay $ 5 * 1000 * 1000

    finalize
    sqlClose sql
