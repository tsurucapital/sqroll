{-# LANGUAGE TypeFamilies #-}
module Database.Sqroll where

import Control.Applicative

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table

class HasTable t where
    type HasTableM m :: * -> *
    table :: NamedTable (HasTableM m) t

instance HasTable Person where
    type HasTableM m = IO
    table = namedTable "people" $ Person
        <$> field "name"    personName
        <*> field "age"     personAge
        <*> field "company" personCompany

data Person = Person
    { personName    :: String
    , personAge     :: Int
    , personCompany :: ForeignKey Int
    } deriving (Show)

personTable :: NamedTable IO Person
personTable = table

jasper :: Person
jasper = Person "Jasper" 22 (SqlRowId 123)

denis :: Person
denis = Person "Denis" 24 (SqlRowId 1234)

main :: IO ()
main = do
    sql <- sqlOpen "test.db"
    sqlExecute sql $ tableCreate personTable
    mapM_ (sqlExecute sql) $ tableIndexes personTable

    stmt <- sqlPrepare sql $ tableInsert personTable
    let binder = tablePoke personTable stmt

    binder jasper
    sqlStep stmt
    sqlReset stmt

    binder denis
    sqlStep stmt

    sqlFinalize stmt


    query <- sqlPrepare sql $ tableSelect personTable
    let peeker = tablePeek personTable
    sqlStep query
    person <- peeker query :: IO Person
    print person
    sqlFinalize query

    sqlClose sql
