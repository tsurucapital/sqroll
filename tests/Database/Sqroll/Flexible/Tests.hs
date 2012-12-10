{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Sqroll.Flexible.Tests (
  tests
) where




import Control.Applicative
import Data.ByteString.Char8 ()
import Database.Sqroll
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
