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
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Sqroll.Flexible where

import GHC.Generics (Generic)
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad.State
import Data.List (intercalate)

import Database.Sqroll.Table.Field
import Database.Sqroll.Internal
import Database.Sqroll.Sqlite3

import Foreign.ForeignPtr

newtype Query r a = Query { runQ :: State (QData r) a }
    deriving (Monad, MonadState (QData r))

{- {{{

TODO:
http://www.sqlite.org/lang_aggfunc.html
http://www.sqlite.org/lang_corefunc.html
http://www.sqlite.org/lang_datefunc.html ?????

GROUP BY
LIKE ???


}}} -}



constructQuery :: a ~ (Exp HaskTag t) => Sqroll -> Query a a -> IO (Stmt t ())
constructQuery sqroll constructedResult = do
        let (r, q) = runState (runQ constructedResult) emptyQuery

            rawQuery = concat $ [ "SELECT ", intercalate ", " (collectFields r)
                                , " FROM ", compileJoin (qFrom q)
                                , mkWhere (qWhere q)
                                , mkGroup (qGroup q)
                                , mkHaving (qHaving q)
                                , mkOrder (qOrder q)
                                ]

        putStrLn rawQuery

        stmt <- sqlPrepare (sqrollSql sqroll) rawQuery
        void $ withForeignPtr stmt $ \raw -> bindExp r raw 1
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

        selectPeek :: Exp HaskTag t -> SqlStmt -> Int -> IO (t, Int)
        selectPeek (Pure x) _ n = return (x, n)
        selectPeek (App a b) stmt n = do
            (ar, n')  <- selectPeek a stmt n
            (br, n'') <- selectPeek b stmt n'
            return (ar br , n'')
        selectPeek (RawValue x) _ n = return (x, n)
        selectPeek (JustValue x) _ n = return (Just x, n)

        -- All other Exps are peekable as fields...
        selectPeek AvgExp{} s n = peekResult s n
        selectPeek TableExp{} _ _ = error "TableExp"
        selectPeek DbValue{} s n = peekResult s n
        selectPeek DirExp{} _ _ = error "DirExp"
        selectPeek AddExp{} s n = peekResult s n
        selectPeek SubExp{} s n = peekResult s n
        selectPeek MulExp{} s n = peekResult s n
        selectPeek DivExp{} s n = peekResult s n
        selectPeek CmpExp{} s n = peekResult s n
        selectPeek Cmp_MExp{} s n = peekResult s n
        selectPeek CmpM_Exp{} s n = peekResult s n
        selectPeek CmpMMExp{} s n = peekResult s n
        selectPeek GtExp{} s n = peekResult s n
        selectPeek CountExp{} s n = peekResult s n
        selectPeek SumExp{} s n = peekResult s n
        selectPeek MaxExp{} s n = peekResult s n
        selectPeek MinExp{} s n = peekResult s n
        selectPeek TotalExp{} s n = peekResult s n

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
collectFields (TableExp ti) = [renderPrim (TableExp ti)]
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
collectFields (DirExp a b) = [renderPrim (DirExp a b)]
collectFields (AvgExp a) = [renderPrim (AvgExp a)]
collectFields (CountExp a) = [renderPrim (CountExp a)]
collectFields (MaxExp a) = [renderPrim (MaxExp a)]
collectFields (MinExp a) = [renderPrim (MinExp a)]
collectFields (SumExp a) = [renderPrim (SumExp a)]
collectFields (TotalExp a) = [renderPrim (TotalExp a)]

emptyQuery :: QData r
emptyQuery = QData {..}
    where
        qFrom = error "From was not specified yet"
        qWhere = []
        qOrder = []
        qHaving = []
        qGroup = []

data QData r = QData
    { qFrom :: ActiveJoin
    , qWhere :: [String]
    , qOrder :: [String]
    , qHaving :: [String]
    , qGroup :: [String]
    }

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
    TableExp  :: TableInstance (NamedTable full) -> Exp t full
    DbValue   :: Field a => String -> Exp t a
    AddExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
    SubExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
    DivExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a
    MulExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t a

    CmpExp    :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool

    CmpM_Exp    :: Field a => Exp SqlTag (Maybe a) -> Exp SqlTag a -> Exp t Bool
    Cmp_MExp    :: Field a => Exp SqlTag a -> Exp SqlTag (Maybe a) -> Exp t Bool
    CmpMMExp    :: Field a => Exp SqlTag (Maybe a) -> Exp SqlTag (Maybe a) -> Exp t Bool

    GtExp     :: Field a => Exp SqlTag a -> Exp SqlTag a -> Exp t Bool

    DirExp    :: Field a => Dir -> Exp SqlTag a -> Exp t Dir

    AvgExp   :: Field a => Exp SqlTag a -> Exp t a
    CountExp :: Field a => Exp SqlTag a -> Exp t Int
    MaxExp   :: Field a => Exp SqlTag a -> Exp t a
    MinExp   :: Field a => Exp SqlTag a -> Exp t a
    SumExp   :: Field a => Exp SqlTag a -> Exp t a
    TotalExp :: Field a => Exp SqlTag a -> Exp t Double

    Pure :: a -> Exp HaskTag a
    App  :: Exp HaskTag (a -> b) -> Exp HaskTag a -> Exp HaskTag b

instance Functor (Exp HaskTag) where
    fmap f e = Pure f `App` e

instance Applicative (Exp HaskTag) where
    pure  = Pure
    (<*>) = App

data TableInstance t = TableInstance Int deriving Show

(^.) :: (Field (Component Full tag), IsTag tag)
     => Exp SqlTag Full -> tag -> Exp t (Component Full tag)
(^.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^.) _ _ = error "WAT"

(^?.) :: (Field (Component Full tag), IsTag tag)
      => Exp SqlTag (Maybe Full) -> tag -> Exp t (Maybe (Component Full tag))
(^?.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^?.) _ _ = error "WAT"


type family Component full tag

class IsTag tag where
    type Full
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



var :: Field a => a -> Exp t a
var = RawValue

just :: Field a => a -> Exp t (Maybe a)
just = JustValue


compileJoin :: ActiveJoin -> String
compileJoin = compileJoinR True
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
where_ cond = modify $ \s -> s { qWhere = renderPrim cond : qWhere s }

order_ :: Exp SqlTag Dir -> Query r ()
order_ cond = modify $ \s -> s { qOrder = renderPrim cond : qOrder s }

having_ :: Exp SqlTag Bool -> Query r ()
having_ cond = modify $ \s -> s { qHaving = renderPrim cond : qHaving s }

group_ :: Exp SqlTag Bool -> Query r ()
group_ cond = modify $ \s -> s { qGroup = renderPrim cond : qGroup s }



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
    blankFromInstance n = (TableExp $ TableInstance n) `InnerJoin` (blankFromInstance $ n + 1)
    constructFrom ((TableExp ti) `InnerJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFrom j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (HasTable t, FromMaybe j) => From ((Exp et t) `LeftJoin` j) where
    blankFromInstance n = (TableExp $ TableInstance n) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFrom ((TableExp ti) `LeftJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (HasTable t, FromMaybe j) => FromMaybe ((Exp et (Maybe t)) `LeftJoin` j) where
    blankFromInstanceMaybe n = (TableExp $ TableInstance n) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFromMaybe ((TableExp ti) `LeftJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFromMaybe _ = error "WAT"

instance (HasTable t, FromMaybe j) => FromMaybe ((Exp et (Maybe t)) `InnerJoin` j) where
    blankFromInstanceMaybe n = (TableExp $ TableInstance n) `InnerJoin` (blankFromInstanceMaybe $ n + 1)
    constructFromMaybe ((TableExp ti) `InnerJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFromMaybe _ = error "WAT"

instance HasTable t => FromMaybe (Exp et (Maybe t)) where
    blankFromInstanceMaybe n = TableExp $ TableInstance n
    constructFromMaybe (TableExp ti) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
        in LastJoin {..}
    constructFromMaybe _ = error "WAT"

instance HasTable t => From (Exp et t) where
    blankFromInstance n = TableExp $ TableInstance n
    constructFrom (TableExp ti) =
        let joinTableName = tableName (table :: NamedTable t)
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
bindExp (DirExp _ a) s n = bindExp a s n
bindExp (TableExp _t) _ _ = error "TODO: bind full table"
bindExp (AvgExp t) s n = bindExp t s n
bindExp (CountExp t) s n = bindExp t s n
bindExp (MaxExp t) s n = bindExp t s n
bindExp (MinExp t) s n = bindExp t s n
bindExp (SumExp t) s n = bindExp t s n
bindExp (TotalExp t) s n = bindExp t s n
bindExp (App a b) s n = bindExp a s n >>= bindExp b s
bindExp (Pure _) _ n = return n


renderPrim :: Exp SqlTag v -> String
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
renderPrim (DirExp Asc e) = concat ["(", renderPrim e,  ") ASC"]
renderPrim (DirExp Desc e) = concat ["(", renderPrim e,  ") DESC"]
renderPrim (TableExp _t) = error "TODO: render and bind full set of fields"
renderPrim (AvgExp t) = concat ["AVG (", renderPrim t, ")"]
renderPrim (CountExp t) = concat ["COUNT (", renderPrim t, ")"]
renderPrim (MaxExp t) = concat ["MAX (", renderPrim t, ")"]
renderPrim (MinExp t) = concat ["MIN (", renderPrim t, ")"]
renderPrim (SumExp t) = concat ["SUM (", renderPrim t, ")"]
renderPrim (TotalExp t) = concat ["TOTAL (", renderPrim t, ")"]

renderAct :: Exp SqlTag v1 -> Exp SqlTag v2 -> String -> String
renderAct e1 e2 s = concat ["(", renderPrim e1, s, renderPrim e2, ")"]

infixl 9 ^., ^?.
infixl 7 *., /.
infixl 6 +., -.
infixl 4 ==., >., ?==?, ?==, ==?
