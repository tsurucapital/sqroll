{-# LANGUAGE OverloadedStrings #-}
module Database.Sqroll.Tests
    ( tests
    ) where

import Data.ByteString.Char8 ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Database.Sqroll
import qualified Database.Sqroll.Tests.ModifiedTypes as ModifiedTypes
import Database.Sqroll.Table
import Database.Sqroll.Tests.Types
import Database.Sqroll.Tests.Util

tests :: Test
tests = testGroup "Database.Sqroll.Tests"
    [ testCase "testAppendTailUsers" testAppendTailUsers
    , testCase "testModifiedTypes"   testModifiedTypes
    , testCase "testMaybeField"      testMaybeField
    , testCase "testBeamableField"   testBeamableField
    , testCase "testTupleField"      testTupleField
    , testCase "testAliasTable"      testAliasTable
    , testCase "testTableIndexes"    testTableIndexes
    , testCase "testTableRefers"     testTableRefers
    , testCase "testSqrollByKey"     testSqrollByKey
    ]

testAppendTailUsers :: Assertion
testAppendTailUsers = testAppendTail testUsers

testModifiedTypes :: Assertion
testModifiedTypes = withTmpFile $ \tmpPath -> do
    -- Insert a user type
    sqroll <- sqrollOpen tmpPath
    sqrollAppend_ sqroll $ User "John" "Doe" 32 "kittens"
    sqrollClose sqroll

    -- Read a modified user type
    sqroll' <- sqrollOpen tmpPath
    stmt <- makeSelectStatement sqroll' (Just ModifiedTypes.defaultUser)
    user <- sqrollGetOne stmt

    sqrollClose sqroll'

    -- Check that they are equal, defaults worked, everyone is happy, rainbows,
    -- unicorns etc.
    ModifiedTypes.User "John" "Doe" "m@jaspervdj.be" 32 @=? user


testMaybeField :: Assertion
testMaybeField = testAppendTail testKittens

testBeamableField :: Assertion
testBeamableField = testAppendTail testFooBars

testTupleField :: Assertion
testTupleField = testAppendTail testHasTuples

testAliasTable :: Assertion
testAliasTable = testAppendTail $ map Dog testKittens

testTableIndexes :: Assertion
testTableIndexes =
    ["CREATE INDEX IF NOT EXISTS index_dog_owner_dog ON dog_owner (dog)"] @=?
    tableIndexes (table :: NamedTable DogOwner)

testTableRefers :: Assertion
testTableRefers = do
    ["dog"] @=? tableRefers (table :: NamedTable Dog)    ownerTable
    []      @=? tableRefers (table :: NamedTable Kitten) ownerTable
  where
    ownerTable = table :: NamedTable DogOwner

testSqrollByKey :: Assertion
testSqrollByKey = withTmpSqroll $ \sqroll -> do
    key <- sqrollAppend sqroll $ Dog (Kitten (Just "Quack"))

    sqrollAppend_ sqroll $ DogOwner "Jasper" key
    sqrollAppend_ sqroll $ DogOwner "Marit" (Key $ unKey key + 1)

    stmt <- makeSelectByKeyStatement sqroll Nothing key
    owner <- sqrollGetOne stmt

    DogOwner "Jasper" key @=? owner


testAppendTail :: (Eq a, HasTable a, Show a) => [a] -> Assertion
testAppendTail items = withTmpSqroll $ \sqroll -> do
    mapM_ (sqrollAppend sqroll) items
    stmt <- makeSelectStatement sqroll Nothing
    items' <- sqrollGetList stmt
    items @=? items'
