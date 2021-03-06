{-# LANGUAGE OverloadedStrings #-}
module Database.Sqroll.Table.Naming.Tests
    ( tests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Database.Sqroll.Table.Naming

tests :: Test
tests = testGroup "Database.Sqroll.Table.Naming.Tests"
    [ testCase "testMakeFieldName" testMakeFieldName
    , testCase "testUnCamelCase"   testUnCamelCase
    ]

testMakeFieldName :: Assertion
testMakeFieldName = do
    "first_name" @=? makeFieldName "Person" "_personFirstName"
    "first_name" @=? makeFieldName "Person" "personFirstName"
    "first_name_1" @=? makeFieldName "Person" "personFirstName1"
    "_1"         @=? makeFieldName "Person" "person1"

testUnCamelCase :: Assertion
testUnCamelCase = do
    "person"       @=? unCamelCase "Person"
    "io_ref"       @=? unCamelCase "IORef"
    "foo_bar"      @=? unCamelCase "FooBar"
    "request_http" @=? unCamelCase "RequestHTTP"
    "request_http1" @=? unCamelCase "RequestHTTP1"
