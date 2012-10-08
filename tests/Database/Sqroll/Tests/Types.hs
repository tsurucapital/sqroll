{-# LANGUAGE DeriveGeneric #-}
module Database.Sqroll.Tests.Types
    ( User (..)
    , testUsers
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Generics (Generic)

import Database.Sqroll (HasTable)

data User = User
    { userFirstName :: String
    , userLastName  :: String
    , userAge       :: Int
    , userPassword  :: ByteString
    } deriving (Eq, Generic, Show)

instance HasTable User

testUsers :: [User]
testUsers = map mkUser [0 .. 10]
  where
    mkUser i = User
        ("John" ++ show i) ("Doe" ++ show i) i (B.pack [0 .. fromIntegral i])
