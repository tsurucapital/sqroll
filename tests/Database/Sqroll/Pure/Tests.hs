{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
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
    [ testCase "testRunWrite" testRunWrite
    ]

data Generation = Generation
    { generationNo     :: Int
    , generationOrigin :: String
    } deriving (Eq, Generic, Show)

instance HasTable Generation

newtype Instrument = Instrument
    { instrumentDescription :: String
    } deriving (Eq, Generic, Show)

instance HasTable Instrument

type InstrumentCache = [(Instrument, Key Instrument)]

writeInstrument :: Instrument
                -> (Key Instrument -> [Write InstrumentCache a])
                -> Write InstrumentCache a
writeInstrument instr f =
    WriteKeyCache instr (lookup instr) (\k c -> (instr, k) : c) f

data Bid = Bid
    { bidGeneration :: Key Generation
    , bidInstrument :: Key Instrument
    , bidPrice      :: Int
    } deriving (Eq, Generic, Show)

instance HasTable Bid

testRunWrite :: Assertion
testRunWrite = withTmpScroll $ \sqroll -> do
    c <- runWrite sqroll []
        [ WriteKey (Generation 1 "testgen") $ \genKey ->
            [ writeInstrument (Instrument "cookies") $ \instrKey ->
                [ Write (Bid genKey instrKey 10)
                , Write (Bid genKey instrKey 12)
                ]
            ]
        ]

    c' <- runWrite sqroll c
        [ WriteKey (Generation 4 "alexgen") $ \genKey ->
            [ writeInstrument (Instrument "cookies") $ \instrKey ->
                [ Write (Bid genKey instrKey 20)
                , Write (Bid genKey instrKey 22)
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
