{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# OPTIONS -fno-warn-missing-signatures #-}


module Database.Sqroll.Json.Tests (tests) where

import Data.Text (Text)
import Database.Sqroll.Internal
import Data.Aeson
import Database.Sqroll.Json
import Database.Sqroll.Tests.Util
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import GHC.Generics (Generic)


data Bacon = Bacon { baconOne :: Int
                   , baconTwo :: Double
                   , baconThree :: Text
                   } deriving (Show, Eq, Generic)
instance HasTable Bacon

tests :: Test
tests = testGroup "Database.Sqroll.Json"
            [ testCase "select blob" testSelectBlob
            ]

_unusedOk = (baconOne, baconTwo, baconThree)

testSelectBlob :: Assertion
testSelectBlob = withTmpSqroll $ \sqroll -> do
        let items = [Bacon i 1.0 "bacon" | i <- [1..2]]
            expected = [ object [("two",Number 1.0)
                                         ,("one",Number 1)
                                         ,("three",String "bacon")
                                         ]
                       , object [("two",Number 1.0)
                                         ,("one",Number 2)
                                         ,("three",String "bacon")
                                         ]
                       ]
        mapM_ (sqrollAppend sqroll) items

        stmt <- makeSelectBlob sqroll "bacon"
        result <- sqrollGetList stmt
        expected @=? result
