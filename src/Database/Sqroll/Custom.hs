{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Sqroll.Custom
    ( deriveExtendedQueries
    , makeCustomSelectStatement
    , (==.), (>=.), (>.), (<=.), (<.), (/=.), (&&.), (||.), (!.)
    , Component, Condition, IsTag
    ) where

import Control.Arrow (first)
import Data.Char (toUpper)
import Data.List (intercalate)
import Foreign.ForeignPtr (withForeignPtr)
import Language.Haskell.TH hiding (Stmt)

import Database.Sqroll.Internal (HasTable, Sqroll (sqrollSql), Stmt (..), prepareTable, mkSelectPeek)
import Database.Sqroll.Sqlite3 (SqlStmt, sqlPrepare, sqlBindInt64)
import Database.Sqroll.Table (makeFieldNames, tableSelect)
import Database.Sqroll.Table.Field (fieldPoke, fieldTypes, Field)
import Database.Sqroll.Table.Naming (makeFieldName)


type family Component full tag :: *

class IsTag full tag | tag -> full where
    getFieldInfo :: tag -> String

data Condition full where
    -- logical operations
    C_And :: Condition full -> Condition full -> Condition full
    C_Or  :: Condition full -> Condition full -> Condition full
    C_Not :: Condition full -> Condition full

    -- compare operations
    C_Eq  :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
    C_Gt  :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
    C_Lt  :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
    C_Gte :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
    C_Lte :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
    

-- bunch of user friendly shortcuts to construct conditions-- {{{

infixr 4 ==., >=., >., <=., <., /=.
infixr 3 &&.
infixr 2 ||.

(&&.) :: Condition a -> Condition a -> Condition a
(&&.) = C_And

(||.) :: Condition a -> Condition a -> Condition a
(||.) = C_Or

(!.) :: Condition a -> Condition a
(!.) = C_Not

(==.) :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
(==.) = C_Eq

(/=.) :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
(/=.) tag = C_Not . C_Eq tag

(>.) :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
(>.) = C_Gt

(>=.) :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
(>=.) = C_Gte

(<.) :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
(<.) = C_Lt

(<=.) :: (IsTag full tag, Field (Component full tag)) => tag -> Component full tag -> Condition full
(<=.) = C_Lte-- }}}

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
                iType = ConT ''IsTag `AppT` ConT base' `AppT` ConT name'
                body = LitE . StringL $ makeFieldName (nameBase pref) (nameBase acc)
                dec = FunD 'getFieldInfo [Clause [WildP] (NormalB body) []]
            in InstanceD [] iType [dec]

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

--------------------------------------------------


compileCond :: Condition a -> String
compileCond (C_And c1 c2) = concat ["( ", compileCond c1, " ) AND ( ", compileCond c2, " )"]
compileCond (C_Or  c1 c2) = concat ["( ", compileCond c1, " ) OR ( ", compileCond c2, " )"]
compileCond (C_Not c)     = concat ["NOT ( ", compileCond c, " )"]
compileCond (C_Eq tag val)  = compileCompOp "=" tag val
compileCond (C_Gt tag val)  | simple val = compileCompOp ">" tag val
compileCond (C_Lt tag val)  | simple val = compileCompOp "<" tag val
compileCond (C_Gte tag val) | simple val = compileCompOp ">=" tag val
compileCond (C_Lte tag val) | simple val = compileCompOp "<=" tag val
compileCond _ = error "This type of condition is currently not supported"

simple :: Field a => a -> Bool
simple val = length (fieldTypes val) == 1

compileCompOp :: (Field val, IsTag full tag) => String -> tag -> val -> String
compileCompOp op tag val =
    let names = makeFieldNames (getFieldInfo tag) (length $ fieldTypes val)
        conds = map (\n -> n ++ " " ++ op ++ " :" ++ n) names
    in concat ["( ", intercalate " AND " conds, " )"]

bindCond :: Int -> SqlStmt -> Condition a -> IO ()
bindCond o stmt (C_And c1 c2)   = bindCond o stmt c1 >> bindCond (o + condOffset c1) stmt c2
bindCond o stmt (C_Or c1 c2)    = bindCond o stmt c1 >> bindCond (o + condOffset c1) stmt c2
bindCond o stmt (C_Not c)       = bindCond o stmt c
bindCond o stmt (C_Eq _ val)  = fieldPoke stmt o val
bindCond o stmt (C_Gt _ val)  = fieldPoke stmt o val
bindCond o stmt (C_Lt _ val)  = fieldPoke stmt o val
bindCond o stmt (C_Gte _ val) = fieldPoke stmt o val
bindCond o stmt (C_Lte _ val) = fieldPoke stmt o val

condOffset :: Condition a -> Int
condOffset (C_And c1 c2) = condOffset c1 + condOffset c2
condOffset (C_Or c1 c2) = condOffset c1 + condOffset c2
condOffset (C_Not c) = condOffset c
condOffset (C_Eq _ val) = length $ fieldTypes val
condOffset (C_Gt _ val) = length $ fieldTypes val
condOffset (C_Lt _ val) = length $ fieldTypes val
condOffset (C_Gte _ val) = length $ fieldTypes val
condOffset (C_Lte _ val) = length $ fieldTypes val


-- | With tag types derived using 'deriveExtendedQueries' you can create queries with
-- customized WHERE conditions:
-- > WHERE fooBar > 3 AND fooBaz < 50
--
-- Suppose we have the same datatype as described in 'deriveExtendedQueries' docs.
-- > do let cond = FooBar >= 3  &&. FooBaz <. 50
-- >    stmt <- makeCustomSelectStatement db dflt  cond
--
-- You can use resulting Stmt with all available Stmt processing functions.
makeCustomSelectStatement :: HasTable a => Sqroll -> Maybe a -> Condition a -> IO (Stmt a a)
makeCustomSelectStatement sqroll defaultRecord cond = do
    table' <- prepareTable sqroll defaultRecord
    stmt   <- sqlPrepare (sqrollSql sqroll) (tableSelect table'
                            ++ " WHERE rowid >= ? AND " ++ compileCond cond
                            ++ " ORDER BY rowid")
    withForeignPtr stmt $ \raw -> do
        sqlBindInt64 raw 1 0
        bindCond 2 raw cond
    return $ Stmt (stmt, mkSelectPeek table')
