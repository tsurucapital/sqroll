{-# LANGUAGE DeriveGeneric #-}
module Database.Sqroll.Pure.Tests
    ( tests
    ) where

import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?), (@?=))

import Database.Sqroll
import Database.Sqroll.Pure
import Database.Sqroll.Tests.Util

tests :: Test
tests = testGroup "Database.Sqroll.Tests"
    [ testCase "testRunLog" testRunLog
    ]

data Generation = Generation
    { _generationNo     :: Int
    , _generationOrigin :: String
    } deriving (Eq, Generic, Show)

instance HasTable Generation

newtype Instrument = Instrument
    { _instrumentDescription :: String
    } deriving (Eq, Generic, Show)

instance HasTable Instrument

type InstrumentCache = [(Instrument, Key Instrument)]

writeInstrument :: Instrument
                -> (Key Instrument -> [Log InstrumentCache])
                -> Log InstrumentCache
writeInstrument instr f =
    LogKeyCache instr (lookup instr) (\k c -> (instr, k) : c) f

data Bid = Bid
    { _bidGeneration :: Key Generation
    , _bidInstrument :: Key Instrument
    , _bidPrice      :: Int
    } deriving (Eq, Generic, Show)

instance HasTable Bid

testRunLog :: Assertion
testRunLog = withTmpScroll $ \sqroll -> do
    c <- runLog sqroll []
        [ LogKey (Generation 1 "testgen") $ \genKey ->
            [ writeInstrument (Instrument "cookies") $ \instrKey ->
                [ Log (Bid genKey instrKey 10)
                , Log (Bid genKey instrKey 12)
                ]
            ]
        ]

    c' <- runLog sqroll c
        [ LogKey (Generation 4 "alexgen") $ \genKey ->
            [ writeInstrument (Instrument "cookies") $ \instrKey ->
                [ Log (Bid genKey instrKey 20)
                , Log (Bid genKey instrKey 22)
                ]
            ]
        ]

    -- Check that there's only one item in the cache
    1 @=? length c'

    (generations, _) <- sqrollTail sqroll (Key 0)
    generations @?=
        [ Generation 1 "testgen"
        , Generation 4 "alexgen"
        ]

    (instruments, _) <- sqrollTail sqroll (Key 0)
    instruments @?=
        [ Instrument "cookies"
        ]

    (bids, _) <- sqrollTail sqroll (Key 0)
    bids @?=
        [ Bid (Key 1) (Key 1) 10
        , Bid (Key 1) (Key 1) 12
        , Bid (Key 2) (Key 1) 20
        , Bid (Key 2) (Key 1) 22
        ]
