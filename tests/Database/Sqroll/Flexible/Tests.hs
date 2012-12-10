{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- sample table in the db
data TestTable = TestTable { tFoo :: Int, tBar :: Double } deriving (Show, Generic)

instance HasTable TestTable
$(deriveExtendedQueries ''TestTable)

tests :: Test
tests = testGroup "Database.Sqroll.Flexible"
    [ testCase "testFlexible" testFlexible
    ]

testFlexible :: Assertion
testFlexible = withTmpSqroll $ \sqroll -> do

    sqrollAppend_ sqroll $ TestTable 1   1.0
    sqrollAppend_ sqroll $ TestTable 100 100.0

    stmt <- constructQuery sqroll $ do
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


-- createStmtX should just typecheck

createStmt :: IO ()
createStmt = do
    _ <- constructQuery undefined $ do
        t `LeftJoin` r <- from
        where_ $ (r ^?. TBar) >. just 100
        where_ $ var 100 >. (t ^. TBar)
        on_ ((t ^. TFoo) ==? (r ^?. TFoo))
        order_ $ asc (t ^. TFoo)
        return $ ResultType <$> (t ^. TFoo) <*> (r ^?. TBar +. just 10) <*> (var True) -- +. t ^. TBar *. var 10) <*. (var True)
    return ()

createStmt2 :: IO ()
createStmt2 = do
    _ <- constructQuery undefined $ do
        t1 `InnerJoin` t2 `InnerJoin` t3 <- from
        on_ ((t1 ^. TFoo) ==. (t2 ^. TFoo))
        on_ ((t2 ^. TFoo) ==. (t3 ^. TFoo))
        return $ (,,) <$> (t1 ^. TFoo) <*> (t2 ^. TFoo) <*> (t3 ^. TFoo)
    return ()


createStmt3 :: IO ()
createStmt3 = do
    _ <- constructQuery undefined $ do
        (t1 :: Exp HaskTag TestTable) <- from
        return $ t1
    return ()


createStmt4 :: IO ()
createStmt4 = do
    _ <- constructQuery undefined $ do
        t1 `InnerJoin` t2 `InnerJoin` t3 `InnerJoin` t4 `InnerJoin` t5 `InnerJoin` t6 <- from
        on_ ((t5 ^. TFoo) ==. (t6 ^. TFoo))
        on_ ((t4 ^. TFoo) ==. (t5 ^. TFoo))
        on_ ((t3 ^. TFoo) ==. (t4 ^. TFoo))
        on_ ((t2 ^. TFoo) ==. (t3 ^. TFoo))
        on_ ((t1 ^. TFoo) ==. (t2 ^. TFoo))
        return $ (,,,,,) <$>  (t1 ^. TFoo) <*> (t2 ^. TFoo) <*> (t3 ^. TFoo) <*> (t4 ^. TFoo) <*> (t5 ^. TFoo) <*> (t6 ^. TFoo)
    return ()

createStmt5 :: IO ()
createStmt5 = do
    _ <- constructQuery undefined $ do
        t1 `LeftJoin` t2 `LeftJoin` t3 `InnerJoin` t4 <- from
        return $ (,,,) <$> (t1 ^. TFoo) <*> (t2 ^?. TFoo) <*> (t3 ^?. TFoo) <*> t4 ^?. TFoo
    return ()

_unused_ok :: IO ()
_unused_ok = do
    _ <- undefined $ (createStmt, createStmt2, createStmt3, createStmt4, createStmt5)
    _ <- undefined $ (tFoo, tBar, foo, bar, baz)

    return ()

-- sample result datatype
data ResultType = ResultType { foo :: Int, bar :: Maybe Double, baz :: Bool } deriving (Eq, Show)
