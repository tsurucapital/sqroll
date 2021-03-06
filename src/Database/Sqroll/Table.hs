{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Sqroll.Table
    ( -- * Types
      Table (..)
    , NamedTable (..)
    , FieldInfo (..)

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
    , tablePeekFrom
    , tablePeekFromMaybe
    , tableRefers
    , tableMakeDefaults
    , tableFields

      -- * Useful if you want to access raw fields
    , makeFieldNames
    ) where

import Control.Arrow (first)
import Control.Monad
import Data.List (intercalate)

import Database.Sqroll.Sqlite3
import Database.Sqroll.Table.Field

data FieldInfo t a = FieldInfo
    { fieldName    :: String
    , fieldExtract :: t -> a
    }

fieldNames :: forall t a. Field a => FieldInfo t a -> [String]
fieldNames fi = makeFieldNames (fieldName fi) (length $ fieldTypes (undefined :: a))

makeFieldNames :: String -> Int -> [String]
makeFieldNames base 1 = [base]
makeFieldNames base n = [base ++ "_" ++ show i | i <- [0 .. n-1]]

fieldColumns' :: forall t a. Field a => FieldInfo t a -> Int
fieldColumns' _ = length $ fieldTypes (undefined :: a)

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
    fieldName' fi = zip (fieldNames fi) (fieldTypes (undefined :: a))

tableCreate :: NamedTable t -> String
tableCreate table =
    "CREATE TABLE IF NOT EXISTS [" ++ tableName table ++ "] (" ++
    intercalate ", " (map makeField $ tableFields table) ++ ")"
  where
    makeField (name, fType) = "[" ++ name ++ "] " ++ sqlTypeToString fType

tableIndexes :: forall t. NamedTable t -> [String]
tableIndexes table = tableFoldMap tableIndex table
  where
    tableIndex :: forall a. Field a => FieldInfo t a -> [String]
    tableIndex fi = do
        index <- fieldIndexes (undefined :: a)
        let idxName = "index_" ++ tableName table ++ "_" ++ fieldName fi
        return $ case index of
            IndexFK _ ->
                "CREATE INDEX IF NOT EXISTS [" ++ idxName ++ "] ON [" ++
                tableName table ++ "] ([" ++ fieldName fi ++ "])"
            IndexUnique ->
                "CREATE UNIQUE INDEX IF NOT EXISTS [unique_" ++ idxName ++
                "] ON [" ++ tableName table ++ "] (" ++
                intercalate ", " ["[" ++ n ++ "]" | n <- fieldNames fi] ++ ")"

tableInsert :: NamedTable t -> String
tableInsert table =
    "INSERT INTO [" ++ tableName table ++ "] ([" ++
    intercalate "], [" (map fst $ tableFields table) ++
    "]) VALUES (" ++
    intercalate ", " (replicate (length fields) "?") ++ ")"
  where
    fields = tableFields table

tableSelect :: NamedTable t -> String
tableSelect table =
    "SELECT rowid, [" ++ intercalate "], [" (map fst $ tableFields table) ++
    "] FROM [" ++ tableName table ++ "]"

tablePoke :: forall t. NamedTable t -> SqlStmt -> t -> IO ()
tablePoke (NamedTable _ table) stmt = \t -> go table t 1 >> return ()
  where
    -- go :: forall a. Field a => FieldInfo t a -> [Int -> t -> IO ()]
    -- go fi = [\n x -> fieldPoke stmt n (fieldExtract fi x)]

    go :: forall a. Table t a -> t -> Int -> IO Int
    go (Map _ t)      x !n = go t x n
    go (Pure _)       _ !n = return n
    go (App ft t)     x !n = do
        n'  <- go ft x n
        n'' <- go t x n'
        return n''
    go (Primitive fi) x !n = do
        fieldPoke stmt n (fieldExtract fi x)
        return (n + fieldColumns' fi)

tablePeek :: forall t. NamedTable t -> SqlStmt -> IO t
tablePeek tbl stmt = fst <$> tablePeekFrom 1 tbl stmt

tablePeekFromMaybe :: forall t. Int -> NamedTable t -> SqlStmt -> IO (Maybe t, Int)
tablePeekFromMaybe startCol t stmt = do
   nullRow <- sqlColumnIsNothing stmt startCol
   if nullRow
        then return (Nothing, 1 + startCol + length (tableFields t))
        else first Just <$> tablePeekFrom (startCol + 1) t stmt

tablePeekFrom :: forall t. Int -> NamedTable t -> SqlStmt -> IO (t, Int)
tablePeekFrom startCol (NamedTable _ table) stmt = go table startCol
  where
    go :: forall a. Table t a -> Int -> IO (a, Int)
    go (Map f t)     !n = liftM (first f) (go t n)
    go (Pure x)      !n = return (x, n)
    go (App ft t)    !n = do
        (f, n')  <- go ft n
        (x, n'') <- go t n'
        return (f x, n'')
    go (Primitive fi) !n = do
        x <- fieldPeek stmt n
        return (x, n + fieldColumns' fi)

-- | Get the names of the columns with which the second table refers to the
-- first table. In principle, there should be only one.
tableRefers :: forall t u. NamedTable u -> NamedTable t -> [String]
tableRefers (NamedTable name _) = tableFoldMap go
  where
    go :: forall a. Field a => FieldInfo t a -> [String]
    go fi =
        [ fieldName fi
        | IndexFK name' <- fieldIndexes (undefined :: a), name == name'
        ]

-- | Check if columns are missing in the database and ensure the defaults are
-- used in those cases
tableMakeDefaults :: forall t. Sql -> Maybe t -> NamedTable t
                  -> IO (NamedTable t)
tableMakeDefaults sql defaultRecord (NamedTable name table) = do
    present <- sqlTableColumns sql name
    return $ NamedTable name $ go present table
  where
    go :: forall a. [String] -> Table t a -> Table t a
    go p (Map f t)  = Map f (go p t)
    go _ (Pure x)   = Pure x
    go p (App ft t) = App (go p ft) (go p t)
    go p (Primitive fi)
        -- All field names need to be present...
        | all (`elem` p) (fieldNames fi) = Primitive fi
        | otherwise                      = Pure $
            maybe fieldDefault (fieldExtract fi) defaultRecord
