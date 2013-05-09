{-# LANGUAGE OverloadedStrings #-}

module Database.Sqroll.Sqlite3.Tests
    ( tests
    ) where

import Data.List (sort)
import Data.Text (Text, pack)
import Foreign (withForeignPtr)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import System.Mem (performGC)

import Database.Sqroll.Sqlite3

tests :: Test
tests = testGroup "Database.Sqroll.Sqlite3.Tests"
    [ testCase "testSqlTableColumns" testSqlTableColumns
    , testCase "testStatementMemorySafety" testStatementMemorySafety
    ]

testSqlTableColumns :: Assertion
testSqlTableColumns = withTmpSql $ \sql -> do
    sqlExecute sql "CREATE TABLE unicorns (rainbows INTEGER, chainsaws DOUBLE);"
    columns <- sqlTableColumns sql "unicorns"
    ["chainsaws", "rainbows"] @=? sort columns

withTmpSql :: (Sql -> IO a) -> IO a
withTmpSql f = do
    sql <- sqlOpen ":memory:" sqlDefaultOpenFlags
    x   <- f sql
    sqlClose sql
    return x


-- | Tests if prepared statements can get memory corruption due to GC.
--
-- This test must be run from ghci.
testStatementMemorySafety :: Assertion
testStatementMemorySafety = do

    -- This test is only useful if run LINE BY LINE in a ghci session,
    -- since it always passes if the code is bound into a function.
    -- The responses from http://www.haskell.org/pipermail/haskell-cafe/2013-May/108176.html did not help.
    -- So to tes this, start your ghci and run this code:

    sql <- sqlOpen ":memory:" sqlDefaultOpenFlags
    sqlExecute sql "CREATE TABLE mytable (mystring STRING);"

    fstmt <- sqlPrepare sql "INSERT INTO mytable (mystring) VALUES (?);"

    let textValue = pack "hello" :: Text

    -- 1 is the prepared param ("?") number (they start with 1)
    withForeignPtr fstmt $ \st -> sqlBindText st 1 textValue
    -- In a flawed implementation, this will cause the value bound to the
    -- statement in the next step to contain garbage.
    performGC
    withForeignPtr fstmt $ \st -> sqlStep_ st >> sqlReset st

    checkStmt <- sqlPrepare sql "SELECT mystring FROM mytable;"

    textInDb <- withForeignPtr checkStmt $ \raw -> do { sqlStep_ raw; t <- sqlColumnText raw 0; sqlReset raw; return (t :: Text) }

    pack "hello" @=? textInDb

    sqlClose sql
