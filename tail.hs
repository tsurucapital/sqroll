{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.Generics (Generic)

import Database.Sqroll

data Person = Person
    { personName :: String
    , personAge  :: Int
    } deriving (Generic, Show)

instance HasTable Person

main :: IO ()
main = do
    sqroll <- sqrollOpen "live.db"
    tail'  <- sqrollTail sqroll (Nothing :: Maybe Person) print

    _ <- forever $ do
        tail'
        threadDelay $ 5 * 1000 * 1000

    sqrollClose sqroll
