{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Sqroll.Table
    ( -- * Types
      Table
    , NamedTable

      -- * Creating tables
    , namedTable
    , field
    , fieldM

      -- * Inspecting tables
    , tableCreate
    , tableIndexes
    , tableInsert
    , tableSelect
    , tablePoke
    , tablePeek
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (intercalate)
import Data.Monoid (Monoid, mappend, mempty)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table.Field

data FieldInfo m t a = FieldInfo
    { fieldName    :: String
    , fieldExtract :: t -> m a
    }

data Table m t f where
    -- Applicative interface
    Map  :: (a -> b) -> Table m t a -> Table m t b
    Pure :: a -> Table m t a
    App  :: Table m t (a -> b) -> Table m t a -> Table m t b

    -- Primitives
    Primitive :: (Field a, Monad m) => FieldInfo m t a -> Table m t a

instance Functor (Table m t) where
    fmap = Map

instance Applicative (Table m t) where
    pure  = Pure
    (<*>) = App

data NamedTable m t = NamedTable
    { tableName :: String
    , tableTree :: Table m t t
    }

namedTable :: String -> Table m t t -> NamedTable m t
namedTable = NamedTable

field :: (Field a, Monad m) => String -> (t -> a) -> Table m t a
field name extract = fieldM name $ return . extract

fieldM :: (Field a, Monad m) => String -> (t -> m a) -> Table m t a
fieldM name extract = Primitive $ FieldInfo name extract

tableFoldMap :: forall m t b. Monoid b
             => (forall a. Field a => FieldInfo m t a -> b)
             -> NamedTable m t
             -> b
tableFoldMap f table = go (tableTree table)
  where
    go :: forall a. Table m t a -> b
    go (Map _ t)      = go t
    go (Pure _)       = mempty
    go (App t1 t2)    = go t1 `mappend` go t2
    go (Primitive fi) = f fi

tableFields :: forall m t. NamedTable m t -> [(String, String)]
tableFields = tableFoldMap fieldName'
  where
    fieldName' :: forall a. Field a => FieldInfo m t a -> [(String, String)]
    fieldName' fi = [(fieldName fi, fieldType (undefined :: a))]

tableCreate :: NamedTable m t -> String
tableCreate table =
    "CREATE TABLE IF NOT EXISTS " ++ tableName table ++ " (" ++
    intercalate ", " (map makeField $ tableFields table) ++ ")"
  where
    makeField (name, type') = name ++ " " ++ type'

tableIndexes :: forall m t. NamedTable m t -> [String]
tableIndexes table = tableFoldMap tableIndex table
  where
    tableIndex :: forall a. Field a => FieldInfo m t a -> [String]
    tableIndex fi = do
        guard $ fieldIndex (undefined :: a)
        let idxName = "index_" ++ tableName table ++ "_" ++ fieldName fi
        return $
            "CREATE INDEX IF NOT EXISTS " ++ idxName ++ " ON " ++
            tableName table ++ " (" ++ fieldName fi ++ ")"

tableInsert :: NamedTable m t -> String
tableInsert table =
    "INSERT INTO " ++ tableName table ++ " (" ++
    intercalate ", " (map fst $ tableFields table) ++
    ") VALUES (" ++
    intercalate ", " (replicate (length fields) "?") ++ ")"
  where
    fields = tableFields table

tableSelect :: NamedTable m t -> String
tableSelect table =
    "SELECT rowid, " ++ intercalate ", " (map fst $ tableFields table) ++
    " FROM " ++ tableName table

tablePoke :: forall m t. MonadIO m => NamedTable m t -> SqlStmt -> (t -> m ())
tablePoke table stmt =
    let pokers = zipWith ($) (tableFoldMap go table) [1 ..]
    in \x -> mapM_ ($ x) pokers
  where
    go :: forall a. Field a => FieldInfo m t a -> [Int -> t -> m ()]
    go fi = [\n x -> fieldExtract fi x >>= liftIO . fieldPoke stmt n]

tablePeek :: forall m t. MonadIO m => NamedTable m t -> SqlStmt -> m t
tablePeek (NamedTable _ table) stmt = do
    (x, _) <- go table 1
    return x
  where
    go :: forall a. Table m t a -> Int -> m (a, Int)
    go (Map f t)     !n = liftM (first f) (go t n)
    go (Pure x)      !n = return (x, n)
    go (App ft t)    !n = do
        (f, n')  <- go ft n
        (x, n'') <- go t n'
        return (f x, n'')
    go (Primitive _) !n = do
        x <- liftIO $ fieldPeek stmt n
        return (x, n + 1)
