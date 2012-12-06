{-# OPTIONS -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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


{-
newtype Stmt a b = Stmt { unStmt :: (SqlFStmt, SqlStmt -> IO (Maybe a)) }
-}

constructQuery :: (HasTable t, a ~ (Result (Exp t))) => Sqroll -> Query a a -> IO (Stmt t ())
constructQuery sqroll constructedResult = do
        (r, q) <- runStateT (runQ constructedResult) emptyQuery

        table' <- prepareTable sqroll Nothing

        let rawQuery = concat $ [ "SELECT 1, ", collectFields r
                                , " FROM ", compileJoin (qFrom q)
                                , mkWhere (qWhere q)
                                , mkOrder (qOrder q)
                                ]

        putStrLn rawQuery

        stmt <- sqlPrepare (sqrollSql sqroll) rawQuery
        void $ withForeignPtr stmt $ \raw -> bindQueryValues r raw 1
        return $ Stmt (stmt, mkSelectPeek table')
    where

        bindQueryValues :: Result (Exp t) -> SqlStmt -> Int -> IO Int
        bindQueryValues (App a b) s n = bindQueryValues a s n >>= bindQueryValues b s
        bindQueryValues Constr {} _ n = return n
        bindQueryValues (Primitive p) s n = bindPrim p s n

        mkWhere :: [String] -> String
        mkWhere [] = []
        mkWhere conds = " WHERE " ++ intercalate " AND " conds

        mkOrder :: [String] -> String
        mkOrder [] = []
        mkOrder conds = " ORDER BY " ++ intercalate " , " conds


collectFields :: Result (Exp t) -> String
collectFields (App Constr{} a) = collectFields a
collectFields (App a b) = concat [collectFields a, ", ", collectFields b]
collectFields (Primitive p) = renderPrim p
collectFields Constr{} = error "WAT"

recPrint :: Result (Exp t) -> String
recPrint (App a b) = concat ["App (",  recPrint a, ") (", recPrint b, ")"]
recPrint (Constr _) = "Constr _"
recPrint (Primitive p) = concat ["Primitive (", renderPrim p, ")"]


emptyQuery :: QData r
emptyQuery = QData {..}
    where
        qResult = error "Result was not created yet"
        qFrom = error "From was not specified yet"
        qWhere = []
        qOrder = []

data QData r = QData
    { qResult :: Result r
    , qFrom :: ActiveJoin
    , qWhere :: [String]
    , qOrder :: [String]
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
    TableExp  :: IsTable full => TableInstance full -> Exp full
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
{-
    MaxExp   :: Num a => Exp a -> Exp a
    SumExp   :: Num a => Exp a -> Exp a
    MinExp   :: Num a => Exp a -> Exp a
    CntExp   :: Exp a -> Exp Int
-}

data Result f where
    -- Applicative interface
    {-
    Map  :: (a -> b) -> Result r a -> Result r b
    Pure :: a -> Result r a
    -}
    App  :: Result (Exp (a -> b)) -> Result (Exp a) -> Result (Exp b)

    -- Primitives
    Constr    :: a -> Result (Exp a)
    Primitive :: Exp a -> Result (Exp a)


(<$.) :: (a -> b) -> Exp a -> Result (Exp b)
(<$.) con e = Constr con <*. e

(<*.) :: Result (Exp (a -> b)) -> Exp a -> Result (Exp b)
(<*.) f a = App f (Primitive a)




data TableInstance t = TableInstance Int deriving Show

(^.) :: (IsTable full, IsTag full tag)
     => Exp full -> tag -> Exp (Component full tag)
(^.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^.) _ _ = error "WAT"

(^?.) :: (IsTable full, IsTag full tag)
     => Exp (Maybe full) -> tag -> Exp (Maybe (Component full tag))
(^?.) (TableExp tbl) tag = DbValue (renderTableTagM tbl tag)
(^?.) _ _ = error "WAT"


grabWhole :: IsTable full => TableInstance full -> Exp full
grabWhole = error "grab whole is not implemented"


type family Component full tag

class IsTag full tag | tag -> full where
    getTagName :: tag -> String

class IsTable t where
    getTableName :: t -> String

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

instance (IsTable t, From j) => From ((Exp t) `InnerJoin` j) where
    blankFromInstance n = (TableExp $ TableInstance n) `InnerJoin` (blankFromInstance $ n + 1)
    constructFrom ((TableExp ti) `InnerJoin` j) =
        let joinTableName = getTableName (undefined :: t)
            joinTableAs = renderTableInstance ti
            joinType = "JOIN"
            joinCondition = Nothing
            joinTo = constructFrom j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance (IsTable t, FromMaybe j) => From ((Exp t) `LeftJoin` j) where
    blankFromInstance n = (TableExp $ TableInstance n) `LeftJoin` (blankFromInstanceMaybe $ n + 1)
    constructFrom ((TableExp ti) `LeftJoin` j) =
        let joinTableName = getTableName (undefined :: t)
            joinTableAs = renderTableInstance ti
            joinType = "LEFT JOIN"
            joinCondition = Nothing
            joinTo = constructFromMaybe j
        in NextJoin {..}
    constructFrom _ = error "WAT"

instance IsTable t => FromMaybe (Exp (Maybe t)) where
    blankFromInstanceMaybe n = TableExp $ TableInstance n
    constructFromMaybe (TableExp ti) =
        let joinTableName = getTableName (undefined :: t)
            joinTableAs = renderTableInstance ti
        in LastJoin {..}
    constructFromMaybe _ = error "WAT"

instance IsTable t => From (Exp t) where
    blankFromInstance n = TableExp $ TableInstance n
    constructFrom (TableExp ti) =
        let joinTableName = getTableName (undefined :: t)
            joinTableAs = renderTableInstance ti
        in LastJoin {..}
    constructFrom _ = error "WAT"



instance IsTable t => IsTable (Maybe t) where
    getTableName _ = getTableName (undefined :: t)



----------------------------------------------------------------------

renderTableTag :: (IsTable full, IsTag full tag) => TableInstance full -> tag -> String
renderTableTag ti tag = concat [ "[", renderTableInstance ti, "].[", getTagName tag, "]" ]

renderTableTagM :: (IsTable full, IsTag full tag) => TableInstance (Maybe full) -> tag -> String
renderTableTagM ti tag = concat [ "[", renderTableInstance ti, "].[", getTagName tag, "]" ]

renderTableInstance :: forall full. IsTable full => TableInstance full -> String
renderTableInstance (TableInstance tid) = "t__" ++ show tid

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
data RRRR = RRRR { foo :: Int, bar :: Maybe Double, baz :: Bool } deriving (Eq, Show, Generic)
instance HasTable RRRR

data Foo = Foo
data Bar = Bar
data Baz = Baz

type instance Component TTTT Foo = Int
type instance Component TTTT Bar = Double
type instance Component TTTT Baz = Bool


instance IsTag TTTT Foo where getTagName _ = "t_foo"
instance IsTag TTTT Bar where getTagName _ = "t_bar"
instance IsTag TTTT Baz where getTagName _ = "t_baz"

instance IsTable TTTT where getTableName _ = "tttt"

