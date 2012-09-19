{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Database.Sqroll.Table.Generic
    ( GNamedTable (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import GHC.Generics

import Database.Sqroll.Table
import Database.Sqroll.Table.Field

class GNamedTable f where
    gNamedTable :: (f a -> t) -> (t -> f a) -> NamedTable t

instance forall d c a. (Datatype d, GTable (M1 C c a)) =>
        GNamedTable (M1 D d (M1 C c a)) where
    gNamedTable mk unmk =
        namedTable (datatypeName value) $ (mk . M1) <$> gTable (unM1 . unmk)
      where
        -- Should not be evalueated since we just use it to get the name of the
        -- datatype.
        value :: M1 D d (M1 C c a) b
        value = undefined
    {-# INLINE gNamedTable #-}

class GTable f where
    gTable :: (t -> f a) -> Table t (f a)

instance GTable f => GTable (M1 C c f) where
    gTable sel = M1 <$> gTable (unM1 . sel)
    {-# INLINE gTable #-}

instance forall a s r. (Field a, Selector s) => GTable (M1 S s (K1 r a)) where
    gTable sel = M1 . K1 <$> field (selName value) (unK1 . unM1 . sel)
      where
        -- Should not be evalueated since we just use it to get the name of the
        -- selector.
        value :: M1 S s (K1 r a) b
        value = undefined
    {-# INLINE gTable #-}

instance (GTable f, GTable g) => GTable (f :*: g) where
    gTable sel = (:*:)
        <$> gTable (\p -> let x :*: _ = sel p in x)
        <*> gTable (\p -> let _ :*: y = sel p in y)
    {-# INLINE gTable #-}
