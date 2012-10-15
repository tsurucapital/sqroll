{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Sqroll.Table
    ( -- * Types
      Table
    , NamedTable (..)

      -- * Creating tables
    , namedTable
    , field
    , mapTable

      -- * Inspecting tables
    , tableCreate
    , tableIndexes
    , tableInsert
    , tableSelect
    , tablePoke
    , tablePeek
    , tableMakeDefaults
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.List (intercalate)
import Data.Monoid (Monoid, mappend, mempty)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table.Field

data FieldInfo t a = FieldInfo
    { fieldName    :: String
    , fieldExtract :: t -> a
    }

data Table t f where
    -- Applicative interface
    Map  :: (a -> b) -> Table t a -> Table t b
    Pure :: a -> Table t a
    App  :: Table t (a -> b) -> Table t a -> Table t b

    -- Primitives
    Primitive :: (Field a) => FieldInfo t a -> Table t a

instance Functor (Table t) where
    fmap = Map

instance Applicative (Table t) where
    pure  = Pure
    (<*>) = App

data NamedTable t = NamedTable
    { tableName :: String
    , tableTree :: Table t t
    }

namedTable :: String -> Table t t -> NamedTable t
namedTable = NamedTable

field :: (Field a) => String -> (t -> a) -> Table t a
field name extract = Primitive $ FieldInfo name extract

mapTable :: forall t u. (t -> u) -> (u -> t) -> Table t t -> Table u u
mapTable mk unmk = Map mk . go
  where
    go :: forall a. Table t a -> Table u a
    go (Map f t)                   = Map f (go t)
    go (Pure x)                    = Pure x
    go (App t1 t2)                 = App (go t1) (go t2)
    go (Primitive (FieldInfo n e)) = Primitive (FieldInfo n (e . unmk))

tableFoldMap :: forall t b. Monoid b
             => (forall a. Field a => FieldInfo t a -> b)
             -> NamedTable t
             -> b
tableFoldMap f table = go (tableTree table)
  where
    go :: forall a. Table t a -> b
    go (Map _ t)      = go t
    go (Pure _)       = mempty
    go (App t1 t2)    = go t1 `mappend` go t2
    go (Primitive fi) = f fi

tableFields :: forall t. NamedTable t -> [(String, SqlType)]
tableFields = tableFoldMap fieldName'
  where
    fieldName' :: forall a. Field a => FieldInfo t a -> [(String, SqlType)]
    fieldName' fi = [(fieldName fi, fieldType (undefined :: a))]

tableCreate :: NamedTable t -> String
tableCreate table =
    "CREATE TABLE IF NOT EXISTS " ++ tableName table ++ " (" ++
    intercalate ", " (map makeField $ tableFields table) ++ ")"
  where
    makeField (name, type') = name ++ " " ++ sqlTypeToString type'

tableIndexes :: forall t. NamedTable t -> [String]
tableIndexes table = tableFoldMap tableIndex table
  where
    tableIndex :: forall a. Field a => FieldInfo t a -> [String]
    tableIndex fi = do
        guard $ fieldIndexed (undefined :: a)
        let idxName = "index_" ++ tableName table ++ "_" ++ fieldName fi
        return $
            "CREATE INDEX IF NOT EXISTS " ++ idxName ++ " ON " ++
            tableName table ++ " (" ++ fieldName fi ++ ")"

tableInsert :: NamedTable t -> String
tableInsert table =
    "INSERT INTO " ++ tableName table ++ " (" ++
    intercalate ", " (map fst $ tableFields table) ++
    ") VALUES (" ++
    intercalate ", " (replicate (length fields) "?") ++ ")"
  where
    fields = tableFields table

tableSelect :: NamedTable t -> String
tableSelect table =
    "SELECT rowid, " ++ intercalate ", " (map fst $ tableFields table) ++
    " FROM " ++ tableName table

tablePoke :: forall t. NamedTable t -> SqlStmt -> (t -> IO ())
tablePoke table stmt =
    let pokers = zipWith ($) (tableFoldMap go table) [1 ..]
    in \x -> mapM_ ($ x) pokers
  where
    go :: forall a. Field a => FieldInfo t a -> [Int -> t -> IO ()]
    go fi = [\n x -> fieldPoke stmt n (fieldExtract fi x)]

tablePeek :: forall t. NamedTable t -> SqlStmt -> IO t
tablePeek (NamedTable _ table) stmt = do
    (x, _) <- go table 1
    return x
  where
    go :: forall a. Table t a -> Int -> IO (a, Int)
    go (Map f t)     !n = liftM (first f) (go t n)
    go (Pure x)      !n = return (x, n)
    go (App ft t)    !n = do
        (f, n')  <- go ft n
        (x, n'') <- go t n'
        return (f x, n'')
    go (Primitive _) !n = do
        x <- fieldPeek stmt n
        return (x, n + 1)

-- | Check if columns are missing in the database and ensure the defaults are
-- used in those cases
tableMakeDefaults :: forall t. Sql -> Maybe t -> NamedTable t
                  -> IO (NamedTable t)
tableMakeDefaults sql defaultRecord (NamedTable name table) = do
    present <- sqlTableColumns sql name
    return $ NamedTable name $ go present table
  where
    go :: forall a. [String] -> Table t a -> Table t a
    go p (Map f t)              = Map f (go p t)
    go _ (Pure x)               = Pure x
    go p (App ft t)             = App (go p ft) (go p t)
    go p (Primitive fi)
        | fieldName fi `elem` p = Primitive fi
        | otherwise             = Pure $
            maybe fieldDefault (fieldExtract fi) defaultRecord
