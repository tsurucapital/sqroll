{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Database.Sqroll.Table.Generic
    ( GNamedTable (..)
    ) where

import GHC.Generics

import Database.Sqroll.Table
import Database.Sqroll.Table.Field
import Database.Sqroll.Table.Naming

class GNamedTable f where
    gNamedTable :: (f a -> t) -> (t -> f a) -> NamedTable t

instance forall d c a. (Datatype d, GTable (M1 C c a)) =>
        GNamedTable (M1 D d (M1 C c a)) where
    gNamedTable mk unmk =
        namedTable (unCamelCase dtn) $ (mk . M1) <$> gTable dtn (unM1 . unmk)
      where
        dtn = datatypeName value

        -- Should not be evalueated since we just use it to get the name of the
        -- datatype.
        value :: M1 D d (M1 C c a) b
        value = undefined
    {-# INLINE gNamedTable #-}

class GTable f where
    gTable :: String -> (t -> f a) -> Table t (f a)

instance GTable f => GTable (M1 C c f) where
    gTable dtn sel = M1 <$> gTable dtn (unM1 . sel)
    {-# INLINE gTable #-}

instance forall a s r. (Field a, Selector s) => GTable (M1 S s (K1 r a)) where
    gTable dtn sel = M1 . K1 <$>
        field (makeFieldName dtn $ selName value) (unK1 . unM1 . sel)
      where
        -- Should not be evalueated since we just use it to get the name of the
        -- selector.
        value :: M1 S s (K1 r a) b
        value = undefined
    {-# INLINE gTable #-}

instance (GTable f, GTable g) => GTable (f :*: g) where
    gTable dtn sel = (:*:)
        <$> gTable dtn (\p -> let x :*: _ = sel p in x)
        <*> gTable dtn (\p -> let _ :*: y = sel p in y)
    {-# INLINE gTable #-}
