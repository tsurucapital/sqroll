{-# LANGUAGE OverloadedStrings #-}
module Database.Sqroll.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Data.ByteString.Char8 ()
import Data.IORef (modifyIORef, newIORef, readIORef)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Database.Sqroll
import qualified Database.Sqroll.Tests.ModifiedTypes as ModifiedTypes
import Database.Sqroll.Sqlite3
import Database.Sqroll.Table
import Database.Sqroll.Tests.Types

tests :: Test
tests = testGroup "Database.Sqroll.Tests"
    [ testCase "testAppendTailUsers" testAppendTailUsers
    , testCase "testModifiedTypes"   testModifiedTypes
    , testCase "testMaybeField"      testMaybeField
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
    append            <- sqrollAppend sqroll Nothing
    append $ User "John" "Doe" 32 "kittens"
    rowid <- sqlLastInsertRowId (sqrollSql sqroll)
    sqrollClose sqroll

    -- Read a modified user type
    sqroll' <- sqrollOpen tmpPath
    select  <- sqrollSelect sqroll' (Just ModifiedTypes.defaultUser)
    user    <- select $ SqlKey rowid
    sqrollClose sqroll'

    -- Check that they are equal, defaults worked, everyone is happy, rainbows,
    -- unicorns etc.
    ModifiedTypes.User "John" "Doe" "m@jaspervdj.be" 32 @=? user

    removeFile tmpPath

testMaybeField :: Assertion
testMaybeField = testAppendTail testKittens

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
    append  <- sqrollAppend sqroll Nothing
    append' <- sqrollAppend sqroll Nothing

    append $ Dog (Kitten (Just "Quack"))
    key <- SqlKey <$> sqlLastInsertRowId (sqrollSql sqroll)

    append' $ DogOwner "Jasper" key
    append' $ DogOwner "Marit" (SqlKey $ unSqlKey key + 1)

    owners <- sqrollByKey sqroll Nothing key
    [DogOwner "Jasper" key] @=? owners

withTmpScroll :: (Sqroll -> IO a) -> IO a
withTmpScroll f = do
    (tmpPath, sqroll) <- sqrollOpenTmp
    x                 <- f sqroll
    sqrollClose sqroll
    removeFile tmpPath
    return x

sqrollOpenTmp :: IO (FilePath, Sqroll)
sqrollOpenTmp = do
    tmpDir <- getTemporaryDirectory
    let tmpPath = tmpDir </> "sqroll-test.db"
    sqroll <- sqrollOpen tmpPath
    return (tmpPath, sqroll)

sqrollTailIORef :: HasTable a => Sqroll -> IO (IO [a])
sqrollTailIORef sqroll = do
    ref    <- newIORef []
    tail'  <- sqrollTail sqroll Nothing (\u -> modifyIORef ref (u :))
    return $ tail' >> reverse <$> readIORef ref

testAppendTail :: (Eq a, HasTable a, Show a) => [a] -> Assertion
testAppendTail items = withTmpScroll $ \sqroll -> do
    tail'  <- sqrollTailIORef sqroll
    append <- sqrollAppend sqroll Nothing
    mapM_ append items
    items' <- tail'
    items @=? items'
