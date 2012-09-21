{-# LANGUAGE DeriveGeneric #-}
import Control.Applicative ((<$>), (<*>))
import GHC.Generics (Generic)

import Database.Sqroll
import Database.Sqroll.Table
import Database.Sqroll.Sqlite3

data TradeLog
    = TradeLog
    { tl_gen :: Int
    , tl_profit :: Double
    , tl_ts :: Int
    } deriving (Eq, Generic, Show)

instance HasTable TradeLog

trades :: [TradeLog]
trades = [TradeLog i (fromIntegral i * 1000) (i + 1000) | i <- [1..]]

main :: IO ()
main = do
    db <- sqlOpen "live.db"
    (massAppend, finalize) <- makeMassAppend db Nothing
    sequence_ . replicate 50 $ massAppend $ take 10000 trades
    finalize
    sqlClose db
