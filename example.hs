{-# LANGUAGE DeriveGeneric #-}
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import GHC.Generics (Generic)

import Database.Sqroll
import Database.Sqroll.Table
import Database.Sqroll.Sqlite3

data Person = Person
    { personName :: String
    , personAge  :: Int
    , personWeight :: Int
    } deriving (Generic, Show)

-- instance HasTable Person

instance HasTable Person where
    table = namedTable "People" $ Person
        <$> field "personName" personName
        <*> field "personAge"  personAge
        <*> field "personWeight"  personAge <? 100

main :: IO ()
main = do
    sql    <- sqlOpen "test.db"
    (sqroll, closeSqroll) <- makeSqroll sql

    sqroll $ Person "Jasper" 23 67

    closeSqroll

    withDefaults <- tableMakeDefaults sql table
    query <- sqlPrepare sql $ tableSelect withDefaults
    let peeker = tablePeek withDefaults
    sqlStep query
    person <- peeker query :: IO Person
    print person
    sqlFinalize query

    sqlClose sql
