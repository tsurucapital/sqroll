{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Sqroll.Tests.Types
    ( User (..)
    , testUsers

    , Kitten (..)
    , testKittens

    , GenericField (..)
    , HasGenericField (..)
    , testHasGenericFields

    , HasTuple (..)
    , testHasTuples

    , Dog (..)
    , DogOwner (..)

    , Sandwich (..)
    , SandwichComponent (..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Database.Sqroll (HasTable (..), Key, aliasTable)
import Database.Sqroll.Table.Field (Field (..))

data User = User
    { userFirstName :: Text
    , userLastName  :: Text
    , userAge       :: Int
    , userPassword  :: ByteString
    } deriving (Eq, Generic, Show)

instance HasTable User

testUsers :: [User]
testUsers = map mkUser [0 .. 10]
  where
    mkUser i = User
        (T.pack $ "John" ++ show i) (T.pack $ "Doe" ++ show i) i (B.pack [0 .. fromIntegral i])

data Kitten = Kitten
    { kittenWoof :: Maybe Text
    } deriving (Eq, Generic, Show)

instance HasTable Kitten

testKittens :: [Kitten]
testKittens = [Kitten Nothing, Kitten (Just "Woof")]

data GenericField = GenericField
    { gfInt   :: Int
    , gfTuple :: (Bool, Bool)
    , gfText  :: Text
    } deriving (Eq, Generic, Show)

instance Field GenericField

data HasGenericField = HasGenericField
    { hasGenericFieldFoo :: GenericField
    } deriving (Eq, Generic, Show)

instance HasTable HasGenericField

testHasGenericFields :: [HasGenericField]
testHasGenericFields =
    [ HasGenericField $ GenericField 1 (False, True) "Herp"
    , HasGenericField $ GenericField 2 (True, True)  "Derp"
    ]

data HasTuple = HasTuple
    { hasTupleFoo :: (Int, Text)
    , hasTupleBar :: ((Text, Int), Bool)
    , hasTupleQux :: (Text, Int, Bool)
    , hasTupleWuu :: (Text, Int, Bool, Text)
    , hasTuple7 :: (Int, Int, Int, Int, Int, Int, Int)
    } deriving (Eq, Generic, Show)

instance HasTable HasTuple

testHasTuples :: [HasTuple]
testHasTuples = return $ HasTuple (3, "hi") (("foo", 10), True) ("oh", 1, False)
    ("hellllo", 1222, True, "<_<")  (1,2,3,4,5,6,7)

newtype Dog = Dog {unDog :: Kitten}
    deriving (Eq, Generic, Show)

instance HasTable Dog where
    table = aliasTable "dog" Dog unDog

data DogOwner = DogOwner
    { dogOwnerName :: Text
    , dogOwnerDog  :: Key Dog
    } deriving (Eq, Generic, Show)

instance HasTable DogOwner

data Sandwich = Sandwich
    { sandwichSize :: Text
    , sandwichOperation :: Text
    , sandwichComponents :: [SandwichComponent]
    } deriving (Eq, Generic, Show)

instance HasTable Sandwich

data SandwichComponent = SandwichComponent
    { sandwichComponentName :: Text
    , sandwichComponentQty  :: Int
    } deriving (Eq, Generic, Show)
instance HasTable SandwichComponent
