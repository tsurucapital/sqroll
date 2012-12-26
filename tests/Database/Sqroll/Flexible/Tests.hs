{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Sqroll.Flexible.Tests (
  tests
) where




import Control.Applicative
import Data.ByteString.Char8 ()
import Database.Sqroll (HasTable, sqrollAppend_, sqrollGetList)
import GHC.Generics (Generic)
import Database.Sqroll.Flexible
import Database.Sqroll.Tests.Util
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Data.Text (Text)

-- sample table in the db
data TestTable = TestTable { tFoo :: Int, tBar :: Double } deriving (Show, Generic)
instance HasTable TestTable
$(deriveExtendedQueries ''TestTable)

data Dept = Dept { deptId :: Int, deptName :: Text } deriving (Show, Eq, Generic)
instance HasTable Dept
$(deriveExtendedQueries ''Dept)

data Empl = Empl { emplId :: Int, emplDeptId :: Int, emplName :: Text } deriving (Show, Eq, Generic)
instance HasTable Empl
$(deriveExtendedQueries ''Empl)

_unusedOk = (tFoo, tBar, deptId, deptName, emplId, emplDeptId, emplName)
_unusedOk2 = (DeptName, EmplId, EmplName, foo, bar, baz)


tests :: Test
tests = testGroup "Database.Sqroll.Flexible"
    [ testCase "testFlexible" testFlexible
    , testCase "testJoins" testJoins
    , testCase "testRebinds" testRebinds
    ]

testFlexible :: Assertion
testFlexible = withTmpSqroll $ \sqroll -> do

    sqrollAppend_ sqroll $ TestTable 1   1.0
    sqrollAppend_ sqroll $ TestTable 100 100.0

    stmt <- makeFlexibleQuery sqroll $ do
        t `LeftJoin` r <- from
--        where_ $ (r ^?. TBar) >. just 100
--        where_ $ var 100 >. (t ^. TBar)
--        on_ ((t ^. TFoo) ==? (r ^?. TFoo))
        order_ $ asc (t ^. TFoo)
        return $ ResultType <$> (t ^. TFoo)
                        <*> (r ^?. TBar +. just 10)
                        <*> (r ^?. TFoo ?== t ^. TFoo)

    result <- sqrollGetList stmt
    let expected = [ ResultType 1   (Just 11.0)  True
                   , ResultType 1   (Just 110.0) False
                   , ResultType 100 (Just 11.0)  False
                   , ResultType 100 (Just 110.0) True
                   ]
    expected @=? result
{-
data Dept = Dept { deptId :: Int, deptName :: Text } deriving (Show, Eq, Generic)
data Empl = Empl { emplId :: Int, emplDeptId :: Int, emplName :: Text } deriving (Show, Eq, Generic)
-}

data Allocation = Allocation Dept (Maybe Empl) deriving (Show, Eq)

testJoins :: Assertion
testJoins = withTmpSqroll $ \sqroll -> do
    let depts = [ Dept 1  "Clerical"
                , Dept 10 "Engineering"
                , Dept 11 "Sales"
                , Dept 12 "Finance"
                ]
        empls = [ Empl 1 1 "Smith"
                , Empl 2 1 "Jones"
                , Empl 3 10 "Robinson"
                , Empl 4 12 "Steinberg"
                ]

        expected = [ Allocation (depts !! 0) (Just $ empls !! 0)
                   , Allocation (depts !! 0) (Just $ empls !! 1)
                   , Allocation (depts !! 1) (Just $ empls !! 2)
                   , Allocation (depts !! 2) Nothing
                   , Allocation (depts !! 3) (Just $ empls !! 3)
                   ]

        expected2 = [ Allocation (depts !! 0) (Just $ empls !! 0)
                    , Allocation (depts !! 0) (Just $ empls !! 1)
                    , Allocation (depts !! 1) (Just $ empls !! 2)
                    , Allocation (depts !! 3) (Just $ empls !! 3)
                    ]
        expected3 = [("Clerical",2),("Engineering",1),("Finance",1)]
        expected4 = [("Clerical",2),("Engineering",1),("Sales", 0),("Finance",1)]

    mapM_ (sqrollAppend_ sqroll) depts
    mapM_ (sqrollAppend_ sqroll) empls

    stmt <- makeFlexibleQuery sqroll $ do
        d `LeftJoin` e <- from
        on_ $ (d ^. DeptId) ==? (e ^?. EmplDeptId)
        return $ Allocation <$> d <*> e
    result <- sqrollGetList stmt
    expected @=? result

    stmt2 <- makeFlexibleQuery sqroll $ do
        d `InnerJoin` e <- from
        on_ $ (d ^. DeptId) ==. (e ^. EmplDeptId)
        return $ Allocation <$> d <*> (Just <$> e)
    result2 <- sqrollGetList stmt2
    expected2 @=? result2

    stmt3 <- makeFlexibleQuery sqroll $ do
        d `InnerJoin` e <- from
        on_ $ (d ^. DeptId) ==. (e ^. EmplDeptId)
        group_ $ d ^. DeptId
        return $ (,) <$> (d ^. DeptName) <*> (count $ e ^. EmplId)
    result3 <- sqrollGetList stmt3
    expected3 @=? result3

    stmt4 <- makeFlexibleQuery sqroll $ do
        d `LeftJoin` e <- from
        on_ $ (d ^. DeptId) ==? (e ^?. EmplDeptId)
        group_ $ d ^. DeptId
        return $ (,) <$> (d ^. DeptName) <*> (count $ e ^?. EmplId)
    result4 <- sqrollGetList stmt4
    expected4 @=? result4

testRebinds :: Assertion
testRebinds = withTmpSqroll $ \sqroll -> do
    let depts = [ Dept 1  "Clerical"
                , Dept 10 "Engineering"
                , Dept 11 "Sales"
                , Dept 12 "Finance"
                ]
        empls = [ Empl 1 1 "Smith"
                , Empl 2 1 "Jones"
                , Empl 3 10 "Robinson"
                , Empl 4 12 "Steinberg"
                ]

    mapM_ (sqrollAppend_ sqroll) depts
    mapM_ (sqrollAppend_ sqroll) empls

    (stmt, setDept) <- makeFlexibleQuery' sqroll $ do
        dept `InnerJoin` empl <- from
        (deptNameParam, deptNameBind) <- newBind "Clerical"

        group_ $ dept ^. DeptId
        on_ $ dept ^. DeptId ==. empl ^. EmplDeptId
        where_ $ dept ^. DeptName ==. deptNameParam

        return $ ((,) <$> (dept ^. DeptName) <*> (count $ empl ^. EmplId), deptNameBind)


    result <- sqrollGetList stmt
    [("Clerical", 2)] @=? result

    setDept "Engineering"
    result2 <- sqrollGetList stmt
    [("Engineering", 1)] @=? result2

    setDept "Sales"
    result3 <- sqrollGetList stmt
    [] @=? result3





-- sample result datatype
data ResultType = ResultType { foo :: Int, bar :: Maybe Double, baz :: Bool } deriving (Eq, Show)
