-- | This module carries types, that slightly differ from the ones in the types
-- module, so we can test migrations between types
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Sqroll.Tests.ModifiedTypes
    ( User (..)
    , defaultUser
    ) where

import Data.Text
import GHC.Generics (Generic)

import Database.Sqroll (HasTable)

data User = User
    { userFirstName :: Text
    , userLastName  :: Text
    , userEmail     :: Text
    , userAge       :: Int
    } deriving (Eq, Generic, Show)

instance HasTable User

defaultUser :: User
defaultUser = User "Jasper" "Van der Jeugt" "m@jaspervdj.be" 22
