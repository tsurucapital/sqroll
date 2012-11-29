{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Sqroll.TH.Tests (
  tests
) where

import Database.Sqroll.Tests.Types
import Database.Sqroll.Tests.Util

import Database.Sqroll.TH
import Database.Sqroll.Sqlite3
import Database.Sqroll

import Control.Exception
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?), assertFailure)

$(initializeAllTablesDec) -- create a function called initializeAllTables

tests :: Test
tests = testGroup "Database.Sqroll.TH.Tests"
    [ testCase "testNoInitializer" testNoInitializer
    , testCase "testTHInitializer" testTHInitializer
    ]

-- check that it fails without the initializer.  Otherwise the second test
-- might not actually be testing anything
testNoInitializer :: Assertion
testNoInitializer = withTmpFile $ \f -> do
    sqroll <- sqrollOpen f
    sqrollClose sqroll
    sqroll' <- sqrollOpenWith f [SqlOpenReadOnly]
    (dogs :: Either SomeException [Dog]) <- try $ sqrollGetList =<< makeSelectStatement sqroll' Nothing
    sqrollClose sqroll'
    either (const $ return ()) (const $ assertFailure "TestNoInitializer had an initialized table") dogs

testTHInitializer :: Assertion
testTHInitializer = withTmpFile $ \f -> do
    sqroll <- sqrollOpen f
    initializeAllTables sqroll
    sqrollClose sqroll

    sqroll' <- sqrollOpenWith f [SqlOpenReadOnly]
    (dogs :: [Dog]) <- sqrollGetList =<< makeSelectStatement sqroll' Nothing
    (kitties :: [Kitten]) <- sqrollGetList =<< makeSelectStatement sqroll' Nothing
    sqrollClose sqroll'
    [] @=? dogs
    [] @=? kitties

