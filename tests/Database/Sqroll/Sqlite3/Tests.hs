module Database.Sqroll.Sqlite3.Tests
    ( tests
    ) where

import Data.List (sort)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Database.Sqroll.Sqlite3

tests :: Test
tests = testGroup "Database.Sqroll.Sqlite3.Tests"
    [ testCase "testSqlTableColumns" testSqlTableColumns
    ]

testSqlTableColumns :: Assertion
testSqlTableColumns = withTmpSql $ \sql -> do
    sqlExecute sql "CREATE TABLE unicorns (rainbows INTEGER, chainsaws DOUBLE);"
    columns <- sqlTableColumns sql "unicorns"
    ["chainsaws", "rainbows"] @=? sort columns

withTmpSql :: (Sql -> IO a) -> IO a
withTmpSql f = do
    sql <- sqlOpen ":memory:"
    x   <- f sql
    sqlClose sql
    return x
