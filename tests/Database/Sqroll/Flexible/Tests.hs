{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

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

tests :: Test
tests = testGroup "Database.Sqroll.Flexible"
    [ testCase "testFlexible" testFlexible
    ]

testFlexible :: Assertion
testFlexible = withTmpSqroll $ \sqroll -> do

    sqrollAppend_ sqroll $ TTTT 1   1.0
    sqrollAppend_ sqroll $ TTTT 100 100.0

    stmt <- constructQuery sqroll $ do
        t `LeftJoin` r <- from
--        where_ $ (r ^?. Bar) >. just 100
--        where_ $ var 100 >. (t ^. Bar)
--        on_ ((t ^. Foo) ==? (r ^?. Foo))
        order_ $ asc (t ^. Foo)
        return $ RRRR <$> (t ^. Foo)
                        <*> (r ^?. Bar +. just 10)
                        <*> (r ^?. Foo ?== t ^. Foo)

    result <- sqrollGetList stmt
    let expected = [ RRRR 1   (Just 11.0)  True
                   , RRRR 1   (Just 110.0) False
                   , RRRR 100 (Just 11.0)  False
                   , RRRR 100 (Just 110.0) True
                   ]
    expected @=? result


-- createStmtX should just typecheck

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


createStmt3 :: IO ()
createStmt3 = do
    _ <- constructQuery undefined $ do
        (t1 :: Exp HaskTag TTTT) <- from
        return $ t1
    return ()


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

_unused_ok :: IO ()
_unused_ok = do
    _ <- undefined $ (createStmt, createStmt2, createStmt3, createStmt4, createStmt5)
    _ <- undefined $ (tFoo, tBar, foo, bar, baz)
    _ <- undefined $ Baz

    return ()


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
