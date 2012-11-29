{-# LANGUAGE TemplateHaskell #-}

module Database.Sqroll.TH (
  initializeAllTablesDec
) where

import Database.Sqroll.Internal (Sqroll, HasTable, sqrollInitializeTable)
import Data.Maybe
import Language.Haskell.TH

-- | create a function 'initializeAllTables :: Sqroll -> IO ()' that, when run,
-- will initialize tables for all datatypes with HasTable instances in scope in
-- the module where this is spliced.
--
-- Currently, only data types with kind * will be included.  So if the
-- following are in scope:
--
-- > instance HasTable Foo
-- > instance HasTable (Bar Int)
-- > instance HasTable (Foo,Foo)
--
-- initializeAllTables will only initialize the table for 'Foo'
initializeAllTablesDec :: Q [Dec]
initializeAllTablesDec = do
    ClassI _ instances <- reify ''HasTable
    sqNm <- newName "sqroll"
    let sqP = varP sqNm
        sqE = varE sqNm
        exprs = catMaybes $ map fromAppT instances

    let allTheExprs = lamE [sqP] $ foldr accExpr [| return () |] exprs
        accExpr x acc = [| sqrollInitializeTable $(sqE) $(x) >> $(acc) |]

    [d| initializeAllTables :: Sqroll -> IO (); initializeAllTables = $(allTheExprs) |]

fromAppT :: Dec -> Maybe ExpQ
fromAppT (InstanceD _ (AppT (ConT cls) (ConT typName)) _) | cls == ''HasTable = Just $ sigE [|undefined|] (conT typName)
-- TODO: match on newtype and data instances
fromAppT _  = Nothing
