{-# OPTIONS -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Sqroll.Flexible
    ( makeFlexibleQuery
    , deriveExtendedQueries
    , (^.), (^?.), (*.), (/.), (+.), (-.), (==.), (?==?), (?==), (==?)
    , (>.), (>=.), (<.), (<=.), (&&.), (||.), (!.)
    , var, just
    , from, on_
    , where_, order_, having_, group_
    , asc, desc
    , InnerJoin (..), LeftJoin (..)
    , count, total, maxval, minval, sumval, avg
    ) where

import Control.Arrow (first)
import Control.Applicative (Applicative (..))
import Control.Monad.State
import Data.Monoid
import Data.Char (toUpper)
import Data.List (intercalate)

import Database.Sqroll.Table (tableFields, tablePeekFrom, tablePeekFromMaybe)
import Database.Sqroll.Table.Field
import Database.Sqroll.Table.Naming (makeFieldName)
import Database.Sqroll.Internal
import Database.Sqroll.Sqlite3

import Foreign.ForeignPtr


import Language.Haskell.TH hiding (Stmt, Exp, runQ)

newtype Query r a = Query { runQ :: State (QData r) a }
    deriving (Monad, MonadState (QData r))

{- {{{

TODO:
http://www.sqlite.org/lang_aggfunc.html
http://www.sqlite.org/lang_corefunc.html
http://www.sqlite.org/lang_datefunc.html ?????

LIKE ???
}}} -}



makeFlexibleQuery :: a ~ (Exp HaskTag t) => Sqroll -> Query a a -> IO (Stmt t ())
makeFlexibleQuery sqroll constructedResult = do
        let (r, q) = runState (runQ constructedResult) emptyQuery

            rawQuery = concat $ [ "SELECT ", intercalate ", " (collectFields r)
                                , " FROM ", compileJoin (qFrom q)
                                , mkWhere (fst $ qWhere q)
                                , mkGroup (fst $ qGroup q)
                                , mkHaving (fst $ qHaving q)
                                , mkOrder (fst $ qOrder q)
                                ]

        stmt <- sqlPrepare (sqrollSql sqroll) rawQuery

        withForeignPtr stmt $ \raw -> do

            n0  <- bindExp r raw 1
            n   <- case snd $ qWhere q of
                    Just cond -> bindExp cond raw n0
                    _ -> return n0
            n'  <- case snd $ qGroup q of
                    Just cond -> bindExp cond raw n
                    _ -> return n
            n'' <- case snd $ qHaving q of
                    Just cond -> bindExp cond raw n'
                    _ -> return n
            _   <- case snd $ qOrder q of
                    Just cond -> bindExp cond raw n''
                    _ -> return n
            return ()

        return $ Stmt (stmt, mkPeeker r)
    where
        mkWhere :: [String] -> String
        mkWhere [] = []
        mkWhere conds = " WHERE " ++ intercalate " AND " conds

        mkHaving :: [String] -> String
        mkHaving [] = []
        mkHaving conds = " HAVING " ++ intercalate " AND " conds


        mkGroup :: [String] -> String
        mkGroup [] = []
        mkGroup conds = " GROUP BY " ++ intercalate " , " conds


        mkOrder :: [String] -> String
        mkOrder [] = []
        mkOrder conds = " ORDER BY " ++ intercalate " , " conds

        mkPeeker :: Exp HaskTag t -> SqlStmt -> IO (Maybe t)
        mkPeeker r stmt = do
                hasData <- sqlStep stmt
                result <- if hasData
                    then (Just . fst) `fmap` selectPeek r stmt 0
                    else do sqlReset stmt
                            return Nothing
                return result

        selectPeek :: Exp HaskTag r -> SqlStmt -> Int -> IO (r, Int)
        selectPeek (Pure x) _ n = return (x, n)
        selectPeek (App a b) stmt n = do
            (ar, n')  <- selectPeek a stmt n
            (br, n'') <- selectPeek b stmt n'
            return (ar br , n'')
        selectPeek (RawValue x) _ n = return (x, n)
        selectPeek (JustValue x) _ n = return (Just x, n)

        -- All other Exps are peekable as fields...
        selectPeek AvgExp{} s n = peekResult s n
        selectPeek (TableExp tbl _) s n = tablePeekFrom n tbl s
        selectPeek (MTableExp tbl _) s n = tablePeekFromMaybe n tbl s
        selectPeek DbValue{} s n = peekResult s n
        selectPeek DirExp{} _ _ = error "peek DirExp"
        selectPeek AddExp{} s n = peekResult s n
        selectPeek SubExp{} s n = peekResult s n
        selectPeek MulExp{} s n = peekResult s n
        selectPeek DivExp{} s n = peekResult s n
        selectPeek CmpExp{} s n = peekResult s n
        selectPeek Cmp_MExp{} s n = peekResult s n
        selectPeek CmpM_Exp{} s n = peekResult s n
        selectPeek CmpMMExp{} s n = peekResult s n
        selectPeek GtExp{} s n = peekResult s n
        selectPeek GteExp{} s n = peekResult s n
        selectPeek LtExp{} s n = peekResult s n
        selectPeek LteExp{} s n = peekResult s n
        selectPeek CountExp{} s n = peekResult s n
        selectPeek SumExp{} s n = peekResult s n
        selectPeek MaxExp{} s n = peekResult s n
        selectPeek MinExp{} s n = peekResult s n
        selectPeek TotalExp{} s n = peekResult s n
        selectPeek AndExp{} s n = peekResult s n
        selectPeek OrExp{} s n = peekResult s n
        selectPeek NotExp{} s n = peekResult s n

        peekResult :: Field r => SqlStmt -> Int -> IO (r, Int)
        peekResult  stmt n = do
            p <- fieldPeek stmt n
            return (p, n + length (fieldTypes p))
            -- undefined


collectFields :: Exp t a -> [String]
collectFields (Pure _)  = []
collectFields (App a b) = collectFields a ++ collectFields b
collectFields (RawValue f) = [renderPrim (RawValue f)]
collectFields (JustValue f) = [renderPrim (JustValue f)]
collectFields (TableExp tbl ti) = [renderPrim (TableExp tbl ti)]
collectFields (MTableExp tbl ti) = [renderPrim (MTableExp tbl ti)]
collectFields (DbValue s) = [s]
collectFields (AddExp a b) = [renderPrim (AddExp a b)]
collectFields (SubExp a b) = [renderPrim (SubExp a b)]
collectFields (DivExp a b) = [renderPrim (DivExp a b)]
collectFields (MulExp a b) = [renderPrim (MulExp a b)]
collectFields (CmpExp a b) = [renderPrim (CmpExp a b)]
collectFields (CmpM_Exp a b) = [renderPrim (CmpM_Exp a b)]
collectFields (Cmp_MExp a b) = [renderPrim (Cmp_MExp a b)]
collectFields (CmpMMExp a b) = [renderPrim (CmpMMExp a b)]
collectFields (GtExp a b) = [renderPrim (GtExp a b)]
collectFields (GteExp a b) = [renderPrim (GteExp a b)]
collectFields (LtExp a b) = [renderPrim (LtExp a b)]
collectFields (LteExp a b) = [renderPrim (LteExp a b)]
collectFields (DirExp a b) = [renderPrim (DirExp a b)]
collectFields (AvgExp a) = [renderPrim (AvgExp a)]
collectFields (CountExp a) = [renderPrim (CountExp a)]
collectFields (MaxExp a) = [renderPrim (MaxExp a)]
collectFields (MinExp a) = [renderPrim (MinExp a)]
collectFields (SumExp a) = [renderPrim (SumExp a)]
collectFields (TotalExp a) = [renderPrim (TotalExp a)]
collectFields (AndExp a b) = [renderPrim (AndExp a b)]
collectFields (OrExp a b) = [renderPrim (OrExp a b)]
collectFields (NotExp a) = [renderPrim (NotExp a)]
collectFields (CommaExp a b) = [renderPrim (CommaExp a b)]
collectFields (GroupExp a) = [renderPrim (GroupExp a)]

emptyQuery :: QData r
emptyQuery = QData {..}
    where
        qFrom = error "From was not specified yet"
        qWhere = ([], Nothing)
        qOrder = ([], Nothing)
        qHaving = ([], Nothing)
        qGroup = ([], Nothing)
        qNextAlias = 1

data QData r = QData
    { qFrom :: ActiveJoin
    , qWhere :: ([String], Maybe (Exp SqlTag Bool))
    , qOrder :: ([String], Maybe (Exp SqlTag Dir))
    , qHaving :: ([String], Maybe (Exp SqlTag Bool))
    , qGroup :: ([String], Maybe (Exp SqlTag GroupBy))
    , qNextAlias :: Int
    }

data GroupBy

data ActiveJoin
    = NextJoin
    { joinTableName :: String
    , joinTableAs   :: String
    , joinType      :: String
    , joinCondition :: Maybe String
    , joinTo        :: ActiveJoin
    }
    | LastJoin
    { joinTableName :: String
    , joinTableAs   :: String
    } deriving (Show)

data SqlTag
data HaskTag

data Exp t a where
    RawValue  :: Field a => a -> Exp t a
    JustValue :: Field a => a -> Exp t (Maybe a)
    TableExp  :: HasTable full => NamedTable full -> TableInstance (NamedTable full) -> Exp t full
    MTableExp :: HasTable full => NamedTable full -> TableInstance (NamedTable full) -> Exp t (Maybe full)
    DbValue   :: Field a => String -> Exp t a
    
    -- math
    AddExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
    SubExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
    DivExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
    MulExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a

    -- boolean
    AndExp    :: Exp SqlTag Bool -> Exp SqlTag Bool -> Exp t Bool
    OrExp     :: Exp SqlTag Bool -> Exp SqlTag Bool -> Exp t Bool
    NotExp    :: Exp SqlTag Bool -> Exp t Bool

    -- compare operations
    CmpExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
    CmpM_Exp  :: Field a => Exp SqlTag (Maybe a) -> Exp SqlTag a -> Exp t Bool
    Cmp_MExp  :: Field a => Exp SqlTag a -> Exp SqlTag (Maybe a) -> Exp t Bool
    CmpMMExp  :: Field a => Exp SqlTag (Maybe a) -> Exp SqlTag (Maybe a) -> Exp t Bool

    GtExp     :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
    GteExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
    LtExp     :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
    LteExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool

    DirExp    :: Field a => Dir -> Exp SqlTag a -> Exp t Dir

    -- aggregation
    AvgExp    :: Field a => Exp SqlTag a -> Exp t a
    CountExp  :: Field a => Exp SqlTag a -> Exp t Int
    MaxExp    :: Field a => Exp SqlTag a -> Exp t a
    MinExp    :: Field a => Exp SqlTag a -> Exp t a
    SumExp    :: Field a => Exp SqlTag a -> Exp t a
    TotalExp  :: Field a => Exp SqlTag a -> Exp t Double

    CommaExp  :: Exp SqlTag a -> Exp SqlTag a -> Exp SqlTag a

    GroupExp  :: Field a => Exp SqlTag a -> Exp SqlTag GroupBy

    Pure      :: a -> Exp HaskTag a
    App       :: Exp HaskTag (a -> b) -> Exp HaskTag a -> Exp HaskTag b


count :: Field a => Exp SqlTag a -> Exp t Int
count = CountExp

total :: Field a => Exp SqlTag a -> Exp t Double
total = TotalExp

avg :: Field a => Exp SqlTag a -> Exp t a
avg = AvgExp

maxval :: Field a => Exp SqlTag a -> Exp t a
maxval = MaxExp

minval :: Field a => Exp SqlTag a -> Exp t a
minval = MinExp

sumval :: Field a => Exp SqlTag a -> Exp t a
sumval = SumExp


instance Functor (Exp HaskTag) where
    fmap f e = Pure f `App` e

instance Applicative (Exp HaskTag) where
    pure  = Pure
    (<*>) = App

instance Monoid (Exp SqlTag Bool) where
    mappend a b = a &&. b
    mempty = var True

instance Monoid (Exp SqlTag Dir) where
    mappend a b = a `CommaExp` b
    mempty = asc $ var True

instance Monoid (Exp SqlTag GroupBy) where
    mappend a b = a `CommaExp` b
    mempty = error "WAT?"

data TableInstance t = TableInstance Int deriving Show

(^.) :: (Field (Component (Full tag) tag), IsTag tag)
     => Exp t1 (Full tag) -> tag -> Exp t2 (Component (Full tag) tag)
(^.) (TableExp _ tbl) tag = DbValue (renderTableTag tbl tag)
(^.) _ _ = error "WAT"

(^?.) :: (Field (Component (Full tag) tag), IsTag tag)
      => Exp t1 (Maybe (Full tag)) -> tag -> Exp t2 (Maybe (Component (Full tag) tag))
(^?.) (MTableExp _ tbl) tag = DbValue (renderTableTag tbl tag)
(^?.) _ _ = error "WAT"


type family Component full tag

class IsTag tag where
    type Full t
    getTagName :: tag -> String

(*.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
(*.) = MulExp

(+.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
(+.) = AddExp

(/.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
(/.) = DivExp

(-.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
(-.) = SubExp

(==.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
(==.) = CmpExp

(==?) :: Field a => Exp SqlTag a -> Exp SqlTag (Maybe a) -> Exp t Bool
(==?) = Cmp_MExp

(?==) :: Field a => Exp SqlTag (Maybe a) -> Exp SqlTag a -> Exp t Bool
(?==) = CmpM_Exp

(?==?) :: Field a => Exp SqlTag (Maybe a) -> Exp SqlTag (Maybe a) -> Exp t Bool
(?==?) = CmpMMExp

(>.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
(>.) v1 v2 = GtExp v1 v2

(>=.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
(>=.) v1 v2 = GteExp v1 v2

(<.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
(<.) v1 v2 = LtExp v1 v2

(<=.) :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
(<=.) v1 v2 = LteExp v1 v2

(&&.) :: Exp SqlTag Bool -> Exp SqlTag Bool -> Exp t Bool
(&&.) v1 v2 = AndExp v1 v2

(||.) :: Exp SqlTag Bool -> Exp SqlTag Bool -> Exp t Bool
(||.) v1 v2 = OrExp v1 v2

(!.) :: Exp SqlTag Bool -> Exp t Bool
(!.) v1 = NotExp v1


var :: Field a => a -> Exp t a
var = RawValue

just :: Field a => a -> Exp t (Maybe a)
just = JustValue


compileJoin :: ActiveJoin -> String
compileJoin j@LastJoin{} = concat [ "[", joinTableName j, "] AS ", joinTableAs j ]
compileJoin aj = compileJoinR True aj
    where
        compileJoinR :: Bool -> ActiveJoin -> String
        compileJoinR True j@NextJoin{} =
            concat [ "[", joinTableName j, "] AS "
                   , joinTableAs j, " " ] ++ compileJoinR False j
        compileJoinR False j@NextJoin{} =
            concat [ joinType j, " ["
                   , joinTableName (joinTo j), "] AS "
                   , joinTableAs (joinTo j), " "
                   , maybe "" ("ON " ++) (joinCondition j)
                   ] ++ " " ++ compileJoinR False (joinTo j)
        compileJoinR _ LastJoin{} = ""

from :: From f => Query r f
from = do
    let r = blankFromInstance 1
    modify $ \s -> s { qFrom = constructFrom r }
    return r

where_ :: Exp SqlTag Bool -> Query r ()
where_ cond = modify $ \s -> s { qWhere = addClause cond (qWhere s) }

order_ :: Exp SqlTag Dir -> Query r ()
order_ cond = modify $ \s -> s { qOrder = addClause cond (qOrder s) }

having_ :: Exp SqlTag Bool -> Query r ()
having_ cond = modify $ \s -> s { qHaving = addClause cond (qHaving s) }

group_ :: Field a => Exp SqlTag a -> Query r ()
group_ cond = modify $ \s -> s { qGroup = addClause (GroupExp cond) (qGroup s) }

addClause :: Monoid (Exp SqlTag a)
    => Exp SqlTag a
    -> ([String], Maybe (Exp SqlTag a))
    -> ([String], Maybe (Exp SqlTag a))
addClause newCond (strs, cond) = (renderPrim newCond : strs, cond `mappend` Just newCond)



data Dir = Asc | Desc

asc :: Field a => Exp SqlTag a -> Exp t Dir
asc = DirExp Asc

desc :: Field a => Exp SqlTag a -> Exp t Dir
desc = DirExp Desc


on_ :: Exp SqlTag Bool -> Query r ()
on_ cond = modify $ \s -> s { qFrom = intoLastFree (qFrom s) (renderPrim cond) }
    where
        intoLastFree :: ActiveJoin -> String -> ActiveJoin
        intoLastFree LastJoin{} _ = error "no cond?"
        intoLastFree j c | isLast j = j { joinCondition = Just c }
        intoLastFree j c = j { joinTo = intoLastFree (joinTo j) c }
        
        isLast :: ActiveJoin -> Bool
        isLast (NextJoin _ _ _ _ (NextJoin _ _ _ (Just _) _)) = True
        isLast (NextJoin _ _ _ _ (LastJoin{})) = True
        isLast _ = False

-- In terms of Sqlite INNER JOIN without specified ON is the same as CROSS JOIN
data InnerJoin a b = a `InnerJoin` b deriving (Show)
data LeftJoin  a b = a `LeftJoin`  b deriving (Show)

infixr 2 `InnerJoin`, `LeftJoin`

class From f where
    blankFromInstance :: Int -> f
    constructFrom :: f -> ActiveJoin

class FromMaybe f where
    blankFromInstanceMaybe :: Int -> f
    constructFromMaybe :: f -> ActiveJoin

instance (HasTable t, From j) => From ((Exp et t) `InnerJoin` j) where
    blankFromInstance n = (TableExp (table :: NamedTable t) (TableInstance n)) `InnerJoin` (blankFromInstance $ n + 1)
    constructFrom ((TableExp tbl ti) `InnerJoin` j) =
        let joinTableName = tableName tbl
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFrom j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (HasTable t, FromMaybe j) => From ((Exp et t) `LeftJoin` j) where
    blankFromInstance n = (TableExp (table :: NamedTable t) (TableInstance n)) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFrom ((TableExp tbl ti) `LeftJoin` j) =
        let joinTableName = tableName tbl
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (HasTable t, FromMaybe j) => FromMaybe ((Exp et (Maybe t)) `LeftJoin` j) where
    blankFromInstanceMaybe n = (MTableExp (table :: NamedTable t) (TableInstance n)) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFromMaybe ((MTableExp tbl ti) `LeftJoin` j) =
        let joinTableName = tableName tbl
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFromMaybe _ = error "WAT"

instance (HasTable t, FromMaybe j) => FromMaybe ((Exp et (Maybe t)) `InnerJoin` j) where
    blankFromInstanceMaybe n = (MTableExp (table :: NamedTable t) (TableInstance n)) `InnerJoin` (blankFromInstanceMaybe $ n + 1)
    constructFromMaybe ((MTableExp tbl ti) `InnerJoin` j) =
        let joinTableName = tableName tbl
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFromMaybe _ = error "WAT"

instance HasTable t => FromMaybe (Exp et (Maybe t)) where
    blankFromInstanceMaybe n = MTableExp (table :: NamedTable t) (TableInstance n)
    constructFromMaybe (MTableExp tbl ti) =
        let joinTableName = tableName tbl
            joinTableAs = renderTableInstance ti
        in LastJoin {..}
    constructFromMaybe _ = error "WAT"

instance HasTable t => From (Exp et t) where
    blankFromInstance n = TableExp (table :: NamedTable t) (TableInstance n)
    constructFrom (TableExp tbl ti) =
        let joinTableName = tableName tbl
            joinTableAs = renderTableInstance ti
        in LastJoin {..}
    constructFrom _ = error "WAT"




----------------------------------------------------------------------

renderTableTag :: IsTag tag => TableInstance a -> tag -> String
renderTableTag ti tag = concat [ renderTableInstance ti, ".[", getTagName tag, "]" ]

renderTableInstance :: TableInstance a -> String
renderTableInstance (TableInstance tid) = "[t__" ++ show tid ++ "]"


bindExp :: Exp t v -> SqlStmt -> Int -> IO Int
bindExp (RawValue f) s n = fieldPoke s n f >> return (n + length (fieldTypes f))
bindExp (JustValue f) s n = fieldPoke s n f >> return (n + length (fieldTypes f))
bindExp DbValue{} _ n = return n
bindExp (AddExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (SubExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (MulExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (DivExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (CmpExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (Cmp_MExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (CmpMMExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (CmpM_Exp a b) s n = bindExp a s n >>= bindExp b s
bindExp (GtExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (GteExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (LtExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (LteExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (AndExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (OrExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (NotExp a) s n = bindExp a s n
bindExp (DirExp _ a) s n = bindExp a s n
bindExp (TableExp _ _) _ n = return n
bindExp (MTableExp _ _) _ n = return n
bindExp (AvgExp t) s n = bindExp t s n
bindExp (CountExp t) s n = bindExp t s n
bindExp (MaxExp t) s n = bindExp t s n
bindExp (MinExp t) s n = bindExp t s n
bindExp (SumExp t) s n = bindExp t s n
bindExp (TotalExp t) s n = bindExp t s n
bindExp (App a b) s n = bindExp a s n >>= bindExp b s
bindExp (Pure _) _ n = return n
bindExp (CommaExp a b) s n = bindExp a s n >>= bindExp b s
bindExp (GroupExp a) s n = bindExp a s n


renderPrim :: forall v. Exp SqlTag v -> String
renderPrim (RawValue a) = intercalate "," (map (const "?") $ fieldTypes a)
renderPrim (JustValue a) = intercalate "," (map (const "?") $ fieldTypes a)
renderPrim (DbValue a) = a
renderPrim (AddExp e1 e2) = renderAct e1 e2 "+"
renderPrim (SubExp e1 e2) = renderAct e1 e2 "-"
renderPrim (MulExp e1 e2) = renderAct e1 e2 "*"
renderPrim (DivExp e1 e2) = renderAct e1 e2 "/"
renderPrim (CmpExp e1 e2) = renderAct e1 e2 "="
renderPrim (Cmp_MExp e1 e2) = renderAct e1 e2 "="
renderPrim (CmpMMExp e1 e2) = renderAct e1 e2 "="
renderPrim (CmpM_Exp e1 e2) = renderAct e1 e2 "="
renderPrim (GtExp  e1 e2) = renderAct e1 e2 ">"
renderPrim (GteExp  e1 e2) = renderAct e1 e2 ">="
renderPrim (LtExp  e1 e2) = renderAct e1 e2 "<"
renderPrim (LteExp  e1 e2) = renderAct e1 e2 "<="
renderPrim (AndExp  e1 e2) = renderAct e1 e2 "AND"
renderPrim (OrExp  e1 e2) = renderAct e1 e2 "OR"
renderPrim (NotExp e) = concat ["NOT (", renderPrim e, ")"]
renderPrim (DirExp Asc e) = concat ["(", renderPrim e,  ") ASC"]
renderPrim (DirExp Desc e) = concat ["(", renderPrim e,  ") DESC"]
renderPrim (AvgExp t) = concat ["AVG (", renderPrim t, ")"]
renderPrim (CountExp t) = concat ["COUNT (", renderPrim t, ")"]
renderPrim (MaxExp t) = concat ["MAX (", renderPrim t, ")"]
renderPrim (MinExp t) = concat ["MIN (", renderPrim t, ")"]
renderPrim (SumExp t) = concat ["SUM (", renderPrim t, ")"]
renderPrim (TotalExp t) = concat ["TOTAL (", renderPrim t, ")"]
renderPrim (CommaExp a b) = concat [renderPrim a, ", ", renderPrim b]
renderPrim (GroupExp a) = renderPrim a
renderPrim (TableExp tbl ti) =
     let tiAs = renderTableInstance ti ++ "."
     in intercalate ", " $ map ((tiAs++) . fst) (tableFields tbl)
renderPrim (MTableExp tbl ti) =
     let tiAs = renderTableInstance ti ++ "."
     in tiAs ++ "rowid, " ++ (intercalate ", " $ map ((tiAs++) . fst) (tableFields tbl))

renderAct :: Exp SqlTag v1 -> Exp SqlTag v2 -> String -> String
renderAct e1 e2 s = concat [" ( ", renderPrim e1, s, renderPrim e2, " ) "]

infixl 9 ^., ^?.
infixl 7 *., /.
infixl 6 +., -.
infixl 4 ==., >., ?==?, ?==, ==?




-- | Derive some instances required for extended queries. To use those instances you will have
-- to add following LANGUAGE pragmas: TypeFamilies, MultiParamTypeClasses and TemplateHaskell
-- for deriving itself.
--
-- For every field of given record style datatype with single constructor created one tag
-- datatype with the same name as field accessor, but with first letter capitalized:
--
-- > data Foo { fooBar :: Int, fooBaz :: Double }
--
-- following tags are created:
--
-- > data FooBar = FooBar
-- > data FooBaz = FooBaz
deriveExtendedQueries :: Name -> Q [Dec]
deriveExtendedQueries typeName = do
        typeInfo <- reify typeName

        case typeInfo of
            TyConI (DataD _ name _ [constr] _) ->
                return $ mkDecls Nothing name (getPrimFields constr)

            TyConI (NewtypeD _ name _ ntConstr _) -> do
                (cName, constr) <- unpackNTConstr ntConstr
                return $ mkDecls (Just name) cName (getPrimFields constr)

            _ -> error invalid

    where

        -- I'm unhappy about this mess
        invalid :: String
        invalid = "You must specify datatype with a single constructor with record syntax."

        unpackNTConstr :: Con -> Q (Name, Con)
        unpackNTConstr (RecC _ [(_, _, ConT underlyingType)]) = do
            typeInfo <- reify underlyingType
            case typeInfo of
                TyConI (DataD _ name _ [constr] _) -> return (name, constr)
                _ -> error invalid
        unpackNTConstr _ = error invalid

        mkDecls :: Maybe Name -> Name -> [(String, Type)] -> [Dec]
        mkDecls mNewType name prim =
                let tags = map (mkTagType mNewType . toConstrName . fst) prim
                    compInsts = map (mkCompInst mNewType name . first toConstrName) prim
                    tagInsts = map (mkTagInst mNewType name . mkName . fst) prim
                in tags ++ compInsts ++ tagInsts

        mkTagType :: Maybe Name -> Name -> Dec
        mkTagType mNewType name = let name' = preNT mNewType name
                                  in DataD [] name' [] [NormalC name' []] []

        mkCompInst :: Maybe Name -> Name -> (Name, Type) -> Dec
        mkCompInst mNewType dName (fName, t) =
            let name' = preNT mNewType fName
                base' = maybe dName id mNewType
            in TySynInstD ''Component [ConT base', ConT name'] t

        mkTagInst :: Maybe Name -> Name -> Name -> Dec
        mkTagInst mNewType pref acc =
            let name' = preNT mNewType (toConstrName $ nameBase acc)
                base' = maybe pref id mNewType
                iType = ConT ''IsTag `AppT` ConT name'
                decTp = TySynInstD ''Full [ConT name'] (ConT base')
                body  = LitE . StringL $ makeFieldName (nameBase pref) (nameBase acc)
                decFn = FunD 'getTagName [Clause [WildP] (NormalB body) []]
            in InstanceD [] iType [decTp, decFn]

        getPrimFields :: Con -> [(String, Type)]
        getPrimFields constr =
            case constr of
                RecC _ fields -> map (\(fn, _, tn) -> (nameBase fn, tn)) fields
                _ -> error "Sorry, but only record-type constructors are supported"

        toConstrName :: String -> Name
        toConstrName (c:cs) = mkName (toUpper c : cs)
        toConstrName _ = error "You can't have empty name"

        preNT :: Maybe Name -> Name -> Name
        preNT (Just prefix) name = mkName (nameBase prefix ++ nameBase name)
        preNT Nothing name = name
