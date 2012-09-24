{-# LANGUAGE DeriveGeneric #-}
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Database.Sqroll
import Database.Sqroll.Table
import Database.Sqroll.Sqlite3

data Person = Person
    { personName :: String
    , personAge  :: Int
    , personHash :: ByteString
    } deriving (Generic, Show)

instance HasTable Person

main :: IO ()
main = do
    sqroll <- sqrollOpen "test.db"
    append <- sqrollAppend sqroll Nothing

    append $ Person "Jasper" 23 $ B.pack [0, 1, 2, 3]

    tail' <- sqrollTail sqroll Nothing (print :: Person -> IO ())
    tail'

    sqrollClose sqroll

    {-
    withDefaults <- tableMakeDefaults sqroll (Just tom) table
    query <- sqlPrepare sqroll $ tableSelect withDefaults
    let peeker = tablePeek withDefaults
    sqlStep query
    person <- peeker query :: IO Person
    print person
    sqlFinalize query

    sqlClose sqroll
    -}
