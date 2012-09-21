{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)

import Database.Sqroll

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
    sqroll <- sqrollOpen "live.db"
    insert <- sqrollAppend sqroll Nothing
    sequence_ . replicate 50 $ sqrollTransaction sqroll $
        mapM_ insert $ take 10000 trades
    sqrollClose sqroll
