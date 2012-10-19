{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Sqroll.Tests.Types
    ( User (..)
    , testUsers

    , Kitten (..)
    , testKittens

    , FooBar (..)
    , testFooBars

    , Dog (..)
    , DogOwner (..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Generics (Generic)

import Database.Sqroll (HasTable (..), Key, aliasTable)
import Database.Sqroll.Table.Field (Field)

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

data Kitten = Kitten
    { kittenWoof :: Maybe String
    } deriving (Eq, Generic, Show)

instance HasTable Kitten

testKittens :: [Kitten]
testKittens = [Kitten Nothing, Kitten (Just "Woof")]

data FooBar = FooBar
    { -- Uses the Beamable instance...
      fooBarList :: [Either String Int]
    } deriving (Eq, Generic, Show)

instance Field [Either String Int]

instance HasTable FooBar

testFooBars :: [FooBar]
testFooBars = [FooBar [Left "NANANANANANA", Right 3, Left "sup"]]

newtype Dog = Dog {unDog :: Kitten}
    deriving (Eq, Generic, Show)

instance HasTable Dog where
    table = aliasTable "dog" Dog unDog

data DogOwner = DogOwner
    { dogOwnerName :: String
    , dogOwnerDog  :: Key Dog
    } deriving (Eq, Generic, Show)

instance HasTable DogOwner
