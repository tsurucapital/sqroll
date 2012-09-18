{-# LANGUAGE TypeFamilies #-}
import Control.Applicative ((<$>), (<*>))

import Database.Sqroll
import Database.Sqroll.Table
import Database.Sqroll.Sqlite3

data TradeLog
    = TradeLog
    { tl_gen :: Int
    , tl_profit :: Double
    , tl_ts :: Int
    } deriving (Eq, Show)

instance HasTable TradeLog where
    table = namedTable "trades" $ TradeLog
        <$> field "gen" tl_gen
        <*> field "age" tl_profit
        <*> field "ts"  tl_ts

trades :: [TradeLog]
trades = [TradeLog i (fromIntegral i * 1000) (i + 1000) | i <- [1..]]

insert10k :: Sql -> (TradeLog -> IO ()) -> IO ()
insert10k db insert = do
    sqlExecute db "BEGIN"
    mapM_ insert $ take 10000 trades
    sqlExecute db "COMMIT"

main :: IO ()
main = do
    db <- sqlOpen ":memory:"
    (insert, finalize) <- makeSqroll db
    sequence_ . replicate 50 $ insert10k db insert
    finalize
    sqlClose db
