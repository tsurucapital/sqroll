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
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Sqroll.Flexible where

import GHC.Generics (Generic)
import Control.Monad.Trans
import Control.Monad.State
import Data.List (intercalate)

import Database.Sqroll.Table.Field
import Database.Sqroll.Internal
import Database.Sqroll.Sqlite3

import Foreign.ForeignPtr

newtype Query r a = Query { runQ :: (StateT (QData r) IO) a }
    deriving (Monad, MonadIO, MonadState (QData r))

{- {{{

TODO:
http://www.sqlite.org/lang_aggfunc.html
http://www.sqlite.org/lang_corefunc.html
http://www.sqlite.org/lang_datefunc.html ?????

GROUP BY
LIKE ???


}}} -}



constructQuery :: a ~ (Result (Exp t)) => Sqroll -> Query a a -> IO (Stmt t ())
constructQuery sqroll constructedResult = do
        (r, q) <- runStateT (runQ constructedResult) emptyQuery

        let rawQuery = concat $ [ "SELECT ", collectFields r
                                , " FROM ", compileJoin (qFrom q)
                                , mkWhere (qWhere q)
                                , mkGroup (qGroup q)
                                , mkHaving (qHaving q)
                                , mkOrder (qOrder q)
                                ]

        stmt <- sqlPrepare (sqrollSql sqroll) rawQuery
        void $ withForeignPtr stmt $ \raw -> bindQueryValues r raw 1
        return $ Stmt (stmt, mkPeeker r)
    where

        bindQueryValues :: Result (Exp t) -> SqlStmt -> Int -> IO Int
        bindQueryValues (App a b) s n = bindQueryValues a s n >>= bindQueryValues b s
        bindQueryValues Constr {} _ n = return n
        bindQueryValues (Primitive p) s n = bindPrim p s n

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

        mkPeeker :: Result (Exp t) -> SqlStmt -> IO (Maybe t)
        mkPeeker r stmt = do
                hasData <- sqlStep stmt
                result <- if hasData
                    then (Just . fst) `fmap` selectPeek r stmt 0
                    else do sqlReset stmt
                            return Nothing
                return result

        selectPeek :: Result (Exp t) -> SqlStmt -> Int -> IO (t, Int)
        selectPeek (App a b) stmt n = do
            (ar, n') <- selectPeek a stmt n
            (br, n'') <- selectPeek b stmt n'
            return (ar br , n'')
        selectPeek (Primitive _) stmt n = do
            p <- fieldPeek stmt n
            return (p, n + length (fieldTypes p))
        selectPeek (Constr c) _ n = return (c, n)


collectFields :: Result (Exp t) -> String
collectFields (App Constr{} a) = collectFields a
collectFields (App a b) = concat [collectFields a, ", ", collectFields b]
collectFields (Primitive p) = renderPrim p
collectFields Constr{} = error "WAT"

emptyQuery :: QData r
emptyQuery = QData {..}
    where
        qResult = error "Result was not created yet"
        qFrom = error "From was not specified yet"
        qWhere = []
        qOrder = []
        qHaving = []
        qGroup = []

data QData r = QData
    { qResult :: Result r
    , qFrom :: ActiveJoin
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



data Exp a where
    RawValue  :: Field a => a -> Exp a
    JustValue :: Field a => a -> Exp (Maybe a)
    TableExp  :: TableInstance (NamedTable full) -> Exp full
    DbValue   :: String -> Exp a
    AddExp    :: Exp a -> Exp a -> Exp a
    SubExp    :: Exp a -> Exp a -> Exp a
    DivExp    :: Exp a -> Exp a -> Exp a
    MulExp    :: Exp a -> Exp a -> Exp a

    CmpExp    :: Exp a -> Exp a -> Exp Bool

    CmpM_Exp    :: Exp (Maybe a) -> Exp a -> Exp Bool
    Cmp_MExp    :: Exp a -> Exp (Maybe a) -> Exp Bool
    CmpMMExp    :: Exp (Maybe a) -> Exp (Maybe a) -> Exp Bool

    GtExp     :: Exp a -> Exp a -> Exp Bool

    DirExp    :: Dir -> Exp a -> Exp Dir

    AvgExp   :: Exp a -> Exp a
    CountExp :: Exp a -> Exp Int
    MaxExp   :: Exp a -> Exp a
    MinExp   :: Exp a -> Exp a
    SumExp   :: Exp a -> Exp a
    TotalExp :: Exp a -> Exp Double


data Result f where
    -- Applicative interface
    App  :: Result (Exp (a -> b)) -> Result (Exp a) -> Result (Exp b)

    -- Primitives
    Constr    :: a -> Result (Exp a)
    Primitive :: Field a => Exp a -> Result (Exp a)


(<$.) :: Field a => (a -> b) -> Exp a -> Result (Exp b)
(<$.) con e = Constr con <*. e

(<*.) :: Field a => Result (Exp (a -> b)) -> Exp a -> Result (Exp b)
(<*.) f a = App f (Primitive a)




data TableInstance t = TableInstance Int deriving Show

(^.) :: (IsTag tag) => Exp Full -> tag -> Exp (Component Full tag)
(^.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^.) _ _ = error "WAT"

(^?.) :: (IsTag tag)
     => Exp (Maybe Full) -> tag -> Exp (Maybe (Component Full tag))
(^?.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^?.) _ _ = error "WAT"


grabWhole :: HasTable Full => TableInstance Full -> Exp Full
grabWhole = error "grab whole is not implemented"


type family Component full tag

class IsTag tag where
    type Full
    getTagName :: tag -> String

(*.) :: Exp a -> Exp a -> Exp a
(*.) = MulExp

(+.) :: Exp a -> Exp a -> Exp a
(+.) = AddExp

(/.) :: Exp a -> Exp a -> Exp a
(/.) = DivExp

(-.) :: Exp a -> Exp a -> Exp a
(-.) = SubExp

(==.) :: Exp a -> Exp a -> Exp Bool
(==.) = CmpExp

(==?) :: Exp a -> Exp (Maybe a) -> Exp Bool
(==?) = Cmp_MExp

(?==) :: Exp (Maybe a) -> Exp a -> Exp Bool
(?==) = CmpM_Exp

(?==?) :: Exp (Maybe a) -> Exp (Maybe a) -> Exp Bool
(?==?) = CmpMMExp

(>.) :: Exp a -> Exp a -> Exp Bool
(>.) v1 v2 = GtExp v1 v2



var :: Field a => a -> Exp a
var = RawValue

just :: Field a => a -> Exp (Maybe a)
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

where_ :: Exp Bool -> Query r ()
where_ cond = modify $ \s -> s { qWhere = renderPrim cond : qWhere s }

order_ :: Exp Dir -> Query r ()
order_ cond = modify $ \s -> s { qOrder = renderPrim cond : qOrder s }

having_ :: Exp Bool -> Query r ()
having_ cond = modify $ \s -> s { qHaving = renderPrim cond : qHaving s }

group_ :: Exp Bool -> Query r ()
group_ cond = modify $ \s -> s { qGroup = renderPrim cond : qGroup s }



data Dir = Asc | Desc

asc :: Exp a -> Exp Dir
asc = DirExp Asc

desc :: Exp a -> Exp Dir
desc = DirExp Desc


on_ :: Exp Bool -> Query r ()
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

instance (HasTable t, From j) => From ((Exp t) `InnerJoin` j) where
    blankFromInstance n = (TableExp $ TableInstance n) `InnerJoin` (blankFromInstance $ n + 1)
    constructFrom ((TableExp ti) `InnerJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFrom j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (HasTable t, FromMaybe j) => From ((Exp t) `LeftJoin` j) where
    blankFromInstance n = (TableExp $ TableInstance n) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFrom ((TableExp ti) `LeftJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (HasTable t, FromMaybe j) => FromMaybe ((Exp (Maybe t)) `LeftJoin` j) where
    blankFromInstanceMaybe n = (TableExp $ TableInstance n) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFromMaybe ((TableExp ti) `LeftJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFromMaybe _ = error "WAT"

instance (HasTable t, FromMaybe j) => FromMaybe ((Exp (Maybe t)) `InnerJoin` j) where
    blankFromInstanceMaybe n = (TableExp $ TableInstance n) `InnerJoin` (blankFromInstanceMaybe $ n + 1)
    constructFromMaybe ((TableExp ti) `InnerJoin` j) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFromMaybe _ = error "WAT"

instance HasTable t => FromMaybe (Exp (Maybe t)) where
    blankFromInstanceMaybe n = TableExp $ TableInstance n
    constructFromMaybe (TableExp ti) =
        let joinTableName = tableName (table :: NamedTable t)
            joinTableAs = renderTableInstance ti
        in LastJoin {..}
    constructFromMaybe _ = error "WAT"

instance HasTable t => From (Exp t) where
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


bindPrim :: Exp v -> SqlStmt -> Int -> IO Int
bindPrim (RawValue f) s n = fieldPoke s n f >> return (n + length (fieldTypes f))
bindPrim (JustValue f) s n = fieldPoke s n f >> return (n + length (fieldTypes f))
bindPrim DbValue{} _ n = return n
bindPrim (AddExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (SubExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (MulExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (DivExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (CmpExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (Cmp_MExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (CmpMMExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (CmpM_Exp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (GtExp a b) s n = bindPrim a s n >>= bindPrim b s
bindPrim (DirExp _ a) s n = bindPrim a s n
bindPrim (TableExp _t) _ _ = error "TODO: bind full table"
bindPrim (AvgExp t) s n = bindPrim t s n
bindPrim (CountExp t) s n = bindPrim t s n
bindPrim (MaxExp t) s n = bindPrim t s n
bindPrim (MinExp t) s n = bindPrim t s n
bindPrim (SumExp t) s n = bindPrim t s n
bindPrim (TotalExp t) s n = bindPrim t s n

renderPrim :: Exp v -> String
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

renderAct :: Exp v1 -> Exp v2 -> String -> String
renderAct e1 e2 s = concat ["(", renderPrim e1, s, renderPrim e2, ")"]

----------------------------------------------------------------------

createStmt :: IO ()
createStmt = do
    _ <- constructQuery undefined $ do
        t `LeftJoin` r <- from
        where_ $ (r ^?. Bar) >. just 100
        where_ $ var 100 >. (t ^. Bar)
        on_ ((t ^. Foo) ==? (r ^?. Foo))
        order_ $ asc (t ^. Foo)
        return $ RRRR <$. (t ^. Foo) <*. (r ^?. Bar +. just 10) <*. (var True) -- +. t ^. Bar *. var 10) <*. (var True)
    return ()

createStmt2 :: IO ()
createStmt2 = do
    _ <- constructQuery undefined $ do
        t1 `InnerJoin` t2 `InnerJoin` t3 <- from
        on_ ((t1 ^. Foo) ==. (t2 ^. Foo))
        on_ ((t2 ^. Foo) ==. (t3 ^. Foo))
        return $ Intx3 <$. (t1 ^. Foo) <*. (t2 ^. Foo) <*. (t3 ^. Foo)
    return ()

{-
createStmt3 :: IO ()
createStmt3 = do
    _ <- constructQuery undefined $ do
        (t1 :: Exp TTTT) <- from
        return $ id <$. t1
    return ()
-}

createStmt4 :: IO ()
createStmt4 = do
    _ <- constructQuery undefined $ do
        t1 `InnerJoin` t2 `InnerJoin` t3 `InnerJoin` t4 `InnerJoin` t5 `InnerJoin` t6 <- from
        on_ ((t5 ^. Foo) ==. (t6 ^. Foo))
        on_ ((t4 ^. Foo) ==. (t5 ^. Foo))
        on_ ((t3 ^. Foo) ==. (t4 ^. Foo))
        on_ ((t2 ^. Foo) ==. (t3 ^. Foo))
        on_ ((t1 ^. Foo) ==. (t2 ^. Foo))
        return $ Intx6 <$.  (t1 ^. Foo) <*. (t2 ^. Foo) <*. (t3 ^. Foo) <*. (t4 ^. Foo) <*. (t5 ^. Foo) <*. (t6 ^. Foo)
    return ()

createStmt5 :: IO ()
createStmt5 = do
    _ <- constructQuery undefined $ do
        t1 `LeftJoin` t2 `LeftJoin` t3 `InnerJoin` t4 <- from
        return $ (,,,) <$. (t1 ^. Foo) <*. (t2 ^?. Foo) <*. (t3 ^?. Foo) <*. t4 ^?. Foo
    return ()

data Intx3 = Intx3 Int Int Int deriving (Generic)
instance HasTable Intx3

data Intx6 = Intx6 Int Int Int Int Int Int deriving (Generic)
instance HasTable Intx6





infixl 9 ^., ^?.
infixl 7 *., /.
infixl 6 +., -.
infixl 4 ==., >., ?==?, ?==, ==?
infixl 3 <$., <*.

-- sample table in the db
data TTTT = TTTT { tFoo :: Int, tBar :: Double } deriving (Show, Generic)

instance HasTable TTTT

-- sample result datatype
data RRRR = RRRR { foo :: Int, bar :: Maybe Double, baz :: Bool } deriving (Eq, Show)

data Foo = Foo
data Bar = Bar
data Baz = Baz

type instance Component TTTT Foo = Int
type instance Component TTTT Bar = Double
type instance Component TTTT Baz = Bool


instance IsTag Foo where
    type Full = TTTT
    getTagName _ = "t_foo"

instance IsTag Bar where
    type Full = TTTT
    getTagName _ = "t_bar"

instance IsTag Baz where
    type Full = TTTT
    getTagName _ = "t_baz"
