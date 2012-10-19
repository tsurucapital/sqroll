{-# LANGUAGE OverloadedStrings #-}
module Database.Sqroll.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Data.ByteString.Char8 ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import System.Directory (removeFile)

import Database.Sqroll
import qualified Database.Sqroll.Tests.ModifiedTypes as ModifiedTypes
import Database.Sqroll.Sqlite3
import Database.Sqroll.Table
import Database.Sqroll.Tests.Types
import Database.Sqroll.Tests.Util

tests :: Test
tests = testGroup "Database.Sqroll.Tests"
    [ testCase "testAppendTailUsers" testAppendTailUsers
    , testCase "testModifiedTypes"   testModifiedTypes
    , testCase "testMaybeField"      testMaybeField
    , testCase "testBeamableField"   testBeamableField
    , testCase "testAliasTable"      testAliasTable
    , testCase "testTableIndexes"    testTableIndexes
    , testCase "testTableRefers"     testTableRefers
    , testCase "testSqrollByKey"     testSqrollByKey
    ]

testAppendTailUsers :: Assertion
testAppendTailUsers = testAppendTail testUsers

testModifiedTypes :: Assertion
testModifiedTypes = do
    -- Insert a user type
    (tmpPath, sqroll) <- sqrollOpenTmp
    sqrollAppend sqroll $ User "John" "Doe" 32 "kittens"
    rowid <- sqlLastInsertRowId (sqrollSql sqroll)
    sqrollClose sqroll

    -- Read a modified user type
    sqroll' <- sqrollOpen tmpPath
    sqrollSetDefault sqroll' (Just ModifiedTypes.defaultUser)
    user <- sqrollSelect sqroll' (Key rowid)
    sqrollClose sqroll'

    -- Check that they are equal, defaults worked, everyone is happy, rainbows,
    -- unicorns etc.
    ModifiedTypes.User "John" "Doe" "m@jaspervdj.be" 32 @=? user

    removeFile tmpPath

testMaybeField :: Assertion
testMaybeField = testAppendTail testKittens

testBeamableField :: Assertion
testBeamableField = testAppendTail testFooBars

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
testSqrollByKey = withTmpScroll $ \sqroll -> do
    sqrollAppend sqroll $ Dog (Kitten (Just "Quack"))
    key <- Key <$> sqlLastInsertRowId (sqrollSql sqroll)

    sqrollAppend sqroll $ DogOwner "Jasper" key
    sqrollAppend sqroll $ DogOwner "Marit" (Key $ unKey key + 1)

    owners <- sqrollByKey sqroll Nothing key
    [DogOwner "Jasper" key] @=? owners

testAppendTail :: (Eq a, HasTable a, Show a) => [a] -> Assertion
testAppendTail items = withTmpScroll $ \sqroll -> do
    mapM_ (sqrollAppend sqroll) items
    (items', _) <- sqrollTailList sqroll (Key 0)
    items @=? items'
