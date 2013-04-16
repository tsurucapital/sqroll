{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad
import Criterion.Main
import Data.Text (Text)
import GHC.Generics
import System.Directory (removeFile)

import Database.Sqroll


newtype SimpleInt = SimpleInt Int deriving (Eq, Show, Generic)

instance HasTable SimpleInt



main :: IO ()
main = do
    putStrLn $ unlines
        [ "BENCHMARK NOTE: Creating new sqlite files is slow (~150 ms per sample on a spinning disk)."
        , "Use --samples=N to reduce the time to an acceptable amount."
        ]

    defaultMain
        [ bgroup "environment"
            [ bench "create and delete a file" $ nfIO (writeFile "XX" "!" >> removeFile "XX")
            ]
        , bgroup "sqlite"
            [ bgroup "NOOP"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ \_ -> return ())
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ \_ -> return ())
                ]
            , bgroup "1 SimpleInt no transaction"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentriesNoTransaction 1)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentriesNoTransaction 1)
                ]
            , bgroup "3 SimpleInt no transaction"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentriesNoTransaction 3)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentriesNoTransaction 3)
                ]
            , bgroup "1 SimpleInt"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentries 1)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentries 1)
                ]
            , bgroup "3 SimpleInt"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentries 3)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentries 3)
                ]
            , bgroup "1000 SimpleInt"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentries 1000)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentries 1000)
                ]
            , bgroup "10000 SimpleInt"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentries 10000)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentries 10000)
                ]
            , bgroup "100000 SimpleInt"
                [ bench "sqrollOpen/Close" $ nfIO (openCloseBenchmarkFile $ writeNentries 100000)
                , bench "withSqroll"       $ nfIO (withBenchmarkFile      $ writeNentries 100000)
                ]
            ]
        ]

-- NOTE:
-- Deleting/recreating the .db file for each benchmark is slow.
-- It takes around 150ms on a spinning disk to create a new, empty .db file,
-- both with sqroll and sqlite3 CLI:
--   time sqlite3 test.db "create table t1 (t1key INTEGER PRIMARY KEY,data TEXT,num double,timeEnter DATE);"
--
-- However, we have to remove to remove the .db files after each benchmark to get independent results.

openCloseBenchmarkFile :: (Sqroll -> IO ()) -> IO ()
openCloseBenchmarkFile f = do
    sqroll <- sqrollOpen "benchmark.db"
    f sqroll
    sqrollClose sqroll
    removeFile "benchmark.db"


withBenchmarkFile :: (Sqroll -> IO ()) -> IO ()
withBenchmarkFile f = withSqroll "benchmark.db" f >> removeFile "benchmark.db"


writeNentriesNoTransaction :: Int -> Sqroll -> IO ()
writeNentriesNoTransaction n sqroll = forM_ [1..n] $ (sqrollAppend_ sqroll . SimpleInt)


writeNentries :: Int -> Sqroll -> IO ()
writeNentries n sqroll = sqrollTransaction sqroll $ writeNentriesNoTransaction n sqroll
