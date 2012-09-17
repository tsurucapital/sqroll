{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Sqroll.Table where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Arrow (first)
import Data.List (intercalate)

import Database.Sqroll.Table.Field
import Database.Sqroll.Table.Sqlite3

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

enumField :: (Enum a, Monad m) => String -> (t -> a) -> Table m t a
enumField name extract = toEnum <$> field name (fromEnum . extract)

--------------------------------------------------------------------------------

tableFields :: NamedTable m t -> [(String, String)]
tableFields (NamedTable _ table) = go table
  where
    go :: forall m t a. Table m t a -> [(String, String)]
    go (Map _ t)      = go t
    go (Pure _ )      = []
    go (App t1 t2)    = go t1 ++ go t2
    go (Primitive fi) = [(fieldName fi, fieldType (undefined :: a))]

tableCreate :: NamedTable m t -> String
tableCreate table =
    "CREATE TABLE IF NOT EXISTS " ++ tableName table ++ " (" ++
    intercalate ", " (map makeField $ tableFields table) ++ ")"
  where
    makeField (name, type') = name ++ " " ++ type'

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
tablePoke (NamedTable _ table) stmt =
    let pokers = zipWith ($) (go table) [1 ..]
    in \x -> mapM_ ($ x) pokers
  where
    go :: forall a. Table m t a -> [Int -> t -> m ()]
    go (Map _ t)      = go t
    go (Pure _)       = []
    go (App t1 t2)    = go t1 ++ go t2
    go (Primitive fi) =
        [\n x -> fieldExtract fi x >>= liftIO . fieldPoke stmt n]

tablePeek :: forall m t. MonadIO m => NamedTable m t -> SqlStmt -> m t
tablePeek (NamedTable _ table) stmt = do
    (x, _) <- go table 1
    return x
  where
    go :: forall a. Table m t a -> Int -> m (a, Int)
    go (Map f t)      !n = liftM (first f) (go t n)
    go (Pure x)       !n = return (x, n)
    go (App ft t)     !n = do
        (f, n')  <- go ft n
        (x, n'') <- go t n'
        return (f x, n'')
    go (Primitive fi) !n = do
        x <- liftIO $ fieldPeek stmt n
        return (x, n + 1)

{-
toRecord :: forall m t. Monad m => NamedTable m t -> t -> m [(String, String)]
toRecord (NamedTable _ table) x = go table
  where
    go :: forall a. Table m t a -> m [(String, String)]
    go (Map _ t)      = go t
    go (Pure _ )      = return []
    go (App t1 t2)    = liftM2 (++) (go t1) (go t2)
    go (Primitive fi) = do
        extr <- fieldExtract fi x
        return [(fieldName fi, fieldShow extr)]

fromRecord :: forall m t. Monad m => NamedTable m t -> [(String, String)] -> m t
fromRecord (NamedTable _ table) record = go table
  where
    go :: forall a. Table m t a -> m a
    go (Map f t)      = liftM f (go t)
    go (Pure x)       = return x
    go (App ft t)     = do
        f <- go ft
        x <- go t
        return (f x)
    go (Primitive fi) = case lookup (fieldName fi) record of
        Nothing  -> error $ "Missing field: " ++ fieldName fi
        Just str -> return $ fieldRead str

printRecord :: [(String, String)] -> IO ()
printRecord = putStrLn . unlines . map (\(x, y) -> x ++ " = " ++ y)

--------------------------------------------------------------------------------

data Hobby = Skate | Surf | Soccer
    deriving (Enum, Show)
-}

data Person = Person
    { personName  :: String
    , personAge   :: Int
    } deriving (Show)

personTable :: NamedTable IO Person
personTable = namedTable "people" $ Person
    <$> field     "name"  personName
    <*> field     "age"   personAge

jasper :: Person
jasper = Person "Jasper" 22

denis :: Person
denis = Person "Denis" 24

{-
main :: IO ()
main = do
    putStrLn "Meta record (CREATE TABLE...):"
    printRecord $ metaRecord personTable

    record <- toRecord personTable jasper

    putStrLn "To record:"
    printRecord record

    putStrLn "From record:"
    print =<< (fromRecord personTable record :: IO Person)
-}

main :: IO ()
main = do
    sql <- sqlOpen "test.db"
    sqlExecute sql $ tableCreate personTable

    stmt <- sqlPrepare sql $ tableInsert personTable
    let binder = tablePoke personTable stmt

    binder jasper
    sqlStep stmt
    sqlReset stmt

    binder denis
    sqlStep stmt

    sqlFinalize stmt


    query <- sqlPrepare sql $ tableSelect personTable
    let peeker = tablePeek personTable
    sqlStep query
    person <- peeker query :: IO Person
    print person
    sqlFinalize query

    sqlClose sql
