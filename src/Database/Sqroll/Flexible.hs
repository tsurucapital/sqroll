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
        selectPeek (TableExp _ti) _ _n = error "TODO: peek whole table?"
        -- All other Exps are peekable as fields...
        selectPeek _ _stmt _n = do
            -- We still need a field constraint here somewhere...
            -- p <- fieldPeek stmt n
            -- return (p, n + length (fieldTypes p))
            undefined


collectFields :: Exp t a -> [String]
collectFields (Pure _)  = []
collectFields (App a b) = collectFields a ++ collectFields b
collectFields (RawValue f) = [renderPrim (RawValue f)]
collectFields (JustValue f) = [renderPrim (JustValue f)]
collectFields (TableExp ti) = [renderPrim (TableExp ti)]
collectFields (DbValue s) = [renderPrim (DbValue s)]
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

emptyQuery :: QData r
emptyQuery = QData {..}
    where
        qFrom = error "From was not specified yet"
        qWhere = []
        qOrder = []

data QData r = QData
    { qFrom :: ActiveJoin
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

data SqlTag
data HaskTag

data Exp t a where
    RawValue  :: Field a => a -> Exp t a
    JustValue :: Field a => a -> Exp t (Maybe a)
    TableExp  :: TableInstance (NamedTable full) -> Exp t full
    DbValue   :: String -> Exp t a
    AddExp    :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
    SubExp    :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
    DivExp    :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
    MulExp    :: Exp SqlTag a -> Exp SqlTag a -> Exp t a

    CmpExp    :: Exp SqlTag a -> Exp SqlTag a -> Exp t Bool

    CmpM_Exp    :: Exp SqlTag (Maybe a) -> Exp SqlTag a -> Exp t Bool
    Cmp_MExp    :: Exp SqlTag a -> Exp SqlTag (Maybe a) -> Exp t Bool
    CmpMMExp    :: Exp SqlTag (Maybe a) -> Exp SqlTag (Maybe a) -> Exp t Bool

    GtExp     :: Exp SqlTag a -> Exp SqlTag a -> Exp t Bool

    DirExp    :: Dir -> Exp SqlTag a -> Exp t Dir
{-
    MaxExp   :: Num a => Exp a -> Exp a
    SumExp   :: Num a => Exp a -> Exp a
    MinExp   :: Num a => Exp a -> Exp a
    CntExp   :: Exp a -> Exp Int
-}

    Pure :: a -> Exp HaskTag a
    App  :: Exp HaskTag (a -> b) -> Exp HaskTag a -> Exp HaskTag b

instance Functor (Exp HaskTag) where
    fmap f e = Pure f `App` e

instance Applicative (Exp HaskTag) where
    pure  = Pure
    (<*>) = App

data TableInstance t = TableInstance Int deriving Show

(^.) :: (IsTag tag) => Exp SqlTag Full -> tag -> Exp t (Component Full tag)
(^.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^.) _ _ = error "WAT"

(^?.) :: (IsTag tag)
     => Exp SqlTag (Maybe Full) -> tag -> Exp t (Maybe (Component Full tag))
(^?.) (TableExp tbl) tag = DbValue (renderTableTag tbl tag)
(^?.) _ _ = error "WAT"


grabWhole :: HasTable Full => TableInstance Full -> Exp t Full
grabWhole = error "grab whole is not implemented"


type family Component full tag

class IsTag tag where
    type Full
    getTagName :: tag -> String

(*.) :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
(*.) = MulExp

(+.) :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
(+.) = AddExp

(/.) :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
(/.) = DivExp

(-.) :: Exp SqlTag a -> Exp SqlTag a -> Exp t a
(-.) = SubExp

(==.) :: Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
(==.) = CmpExp

(==?) :: Exp SqlTag a -> Exp SqlTag (Maybe a) -> Exp t Bool
(==?) = Cmp_MExp

(?==) :: Exp SqlTag (Maybe a) -> Exp SqlTag a -> Exp t Bool
(?==) = CmpM_Exp

(?==?) :: Exp SqlTag (Maybe a) -> Exp SqlTag (Maybe a) -> Exp t Bool
(?==?) = CmpMMExp

(>.) :: Exp SqlTag a -> Exp SqlTag a -> Exp t Bool
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


data Dir = Asc | Desc

asc :: Exp SqlTag a -> Exp t Dir
asc = DirExp Asc

desc :: Exp SqlTag a -> Exp t Dir
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

renderAct :: Exp SqlTag v1 -> Exp SqlTag v2 -> String -> String
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
        return $ RRRR <$> (t ^. Foo) <*> (r ^?. Bar +. just 10) <*> (var True) -- +. t ^. Bar *. var 10) <*. (var True)
    return ()

createStmt2 :: IO ()
createStmt2 = do
    _ <- constructQuery undefined $ do
        t1 `InnerJoin` t2 `InnerJoin` t3 <- from
        on_ ((t1 ^. Foo) ==. (t2 ^. Foo))
        on_ ((t2 ^. Foo) ==. (t3 ^. Foo))
        return $ Intx3 <$> (t1 ^. Foo) <*> (t2 ^. Foo) <*> (t3 ^. Foo)
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
        return $ Intx6 <$>  (t1 ^. Foo) <*> (t2 ^. Foo) <*> (t3 ^. Foo) <*> (t4 ^. Foo) <*> (t5 ^. Foo) <*> (t6 ^. Foo)
    return ()

createStmt5 :: IO ()
createStmt5 = do
    _ <- constructQuery undefined $ do
        t1 `LeftJoin` t2 `LeftJoin` t3 `InnerJoin` t4 <- from
        return $ (,,,) <$> (t1 ^. Foo) <*> (t2 ^?. Foo) <*> (t3 ^?. Foo) <*> t4 ^?. Foo
    return ()

data Intx3 = Intx3 Int Int Int deriving (Generic)
instance HasTable Intx3

data Intx6 = Intx6 Int Int Int Int Int Int deriving (Generic)
instance HasTable Intx6





infixl 9 ^., ^?.
infixl 7 *., /.
infixl 6 +., -.
infixl 4 ==., >., ?==?, ?==, ==?

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
