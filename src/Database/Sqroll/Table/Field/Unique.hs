{-# LANGUAGE ScopedTypeVariables #-}
module Database.Sqroll.Table.Field.Unique
    ( Unique (..)
    ) where

import Control.Applicative ((<$>))

import Database.Sqroll.Table.Field

newtype Unique a = Unique {unUnique :: a}
    deriving (Eq, Ord, Show)

instance forall a. Field a => Field (Unique a) where
    fieldTypes   _ = fieldTypes (undefined :: a)
    fieldIndexes _ = IndexUnique : fieldIndexes (undefined :: a)
    fieldDefault   = Unique fieldDefault

    fieldPoke stmt n (Unique x) = fieldPoke stmt n x
    fieldPeek stmt n            = Unique <$> fieldPeek stmt n
