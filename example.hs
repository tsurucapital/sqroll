import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)

import Database.Sqroll
import Database.Sqroll.Table
import Database.Sqroll.Sqlite3

data Person = Person
    { personName :: String
    , personAge  :: Int
    } deriving (Show)

instance HasTable Person where
    table = namedTable "people" $ Person
        <$> field "name"    personName
        <*> field "age"     personAge

main :: IO ()
main = do
    sql    <- sqlOpen "test.db"
    (sqroll, closeSqroll) <- makeSqroll sql

    sqlExecute sql "BEGIN"
    forM_ [1 .. 1000000] $ \i -> do
        _ <- sqroll $ Person ("Dude " ++ show i) (i `mod` 60)
        return ()
    sqlExecute sql "COMMIT"

    closeSqroll

    {-

    query <- sqlPrepare sql $ tableSelect personTable
    let peeker = tablePeek personTable
    sqlStep query
    person <- peeker query :: IO Person
    print person
    sqlFinalize query

    -}

    sqlClose sql
