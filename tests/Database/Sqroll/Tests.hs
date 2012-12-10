{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Sqroll.Tests
    ( tests
    ) where

import Data.ByteString.Char8 ()
import qualified Data.Text as T
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Database.Sqroll
import qualified Database.Sqroll.Tests.ModifiedTypes as ModifiedTypes
import Database.Sqroll.Table
import Database.Sqroll.Tests.Types
import Database.Sqroll.Tests.Util

tests :: Test
tests = testGroup "Database.Sqroll.Tests"
    [ testCase "testAppendTailUsers" testAppendTailUsers
    , testCase "testModifiedTypes"   testModifiedTypes
    , testCase "testMaybeField"      testMaybeField
    , testCase "testGenericField"    testGenericField
    , testCase "testTupleField"      testTupleField
    , testCase "testUniqueIndex"     testUniqueIndex
    , testCase "testAliasTable"      testAliasTable
    , testCase "testTableIndexes"    testTableIndexes
    , testCase "testTableRefers"     testTableRefers
    , testCase "testSqrollByKey"     testSqrollByKey
    , testCase "testSelectEntity"    testSelectEntity
    , testCase "testSelectNext"      testSelectNext
    , testCase "testFold"            testFold
    , testCase "testFoldAll"         testFoldAll
    , testCase "testListFields"      testListFields
    , testCase "testRebindKey"       testRebindKey
--    , testCase "testCustomQuery"     testCustomQuery
--    , testCase "testCustomQueryNT"   testCustomQueryNT
    ]

testAppendTailUsers :: Assertion
testAppendTailUsers = testAppendTail testUsers

testModifiedTypes :: Assertion
testModifiedTypes = withTmpFile $ \tmpPath -> do
    -- Insert a user type
    sqroll <- sqrollOpen tmpPath
    sqrollAppend_ sqroll $ User "John" "Doe" 32 "kittens"
    sqrollClose sqroll

    -- Read a modified user type
    sqroll' <- sqrollOpen tmpPath
    stmt <- makeSelectStatement sqroll' (Just ModifiedTypes.defaultUser)
    user <- sqrollGetOne stmt

    -- Check that they are equal, defaults worked, everyone is happy, rainbows,
    -- unicorns etc.
    ModifiedTypes.User "John" "Doe" "m@jaspervdj.be" 32 @=? user


testMaybeField :: Assertion
testMaybeField = testAppendTail testKittens

testGenericField :: Assertion
testGenericField = testAppendTail testHasGenericFields

testTupleField :: Assertion
testTupleField = testAppendTail testHasTuples

testUniqueIndex :: Assertion
testUniqueIndex =
    ["CREATE UNIQUE INDEX IF NOT EXISTS " ++
        "[unique_index_has_unique_index_coords] ON " ++
        "[has_unique_index] ([coords_0], [coords_1])"] @=?
    tableIndexes (table :: NamedTable HasUniqueIndex)

testAliasTable :: Assertion
testAliasTable = testAppendTail $ map Dog testKittens

testTableIndexes :: Assertion
testTableIndexes =
    ["CREATE INDEX IF NOT EXISTS [index_dog_owner_dog] " ++
        "ON [dog_owner] ([dog])"] @=?
    tableIndexes (table :: NamedTable DogOwner)

testTableRefers :: Assertion
testTableRefers = do
    ["dog"] @=? tableRefers (table :: NamedTable Dog)    ownerTable
    []      @=? tableRefers (table :: NamedTable Kitten) ownerTable
  where
    ownerTable = table :: NamedTable DogOwner

testSqrollByKey :: Assertion
testSqrollByKey = withTmpSqroll $ \sqroll -> do
    key <- sqrollAppend sqroll $ Dog (Kitten (Just "Quack"))

    sqrollAppend_ sqroll $ DogOwner "Jasper" key
    sqrollAppend_ sqroll $ DogOwner "Marit" (Key $ unKey key + 1)

    stmt <- makeSelectByKeyStatement sqroll Nothing key
    owner <- sqrollGetOne stmt

    DogOwner "Jasper" key @=? owner

testRebindKey :: Assertion
testRebindKey = withTmpSqroll $ \sqroll -> do
    key1 <- sqrollAppend sqroll $ Dog (Kitten (Just "Quack"))
    key2 <- sqrollAppend sqroll $ Dog (Kitten (Just "Quack"))

    sqrollAppend_ sqroll $ DogOwner "Jasper" key1
    sqrollAppend_ sqroll $ DogOwner "Marit"  key2

    stmt <- makeSelectByKeyStatement sqroll Nothing key2
    owner2 <- sqrollGetOne stmt

    sqrollRebindKey stmt (unKey key1)
    owner1 <- sqrollGetOne stmt

    DogOwner "Jasper" key1 @=? owner1
    DogOwner "Marit" key2 @=? owner2

testAppendTail :: (Eq a, HasTable a, Show a) => [a] -> Assertion
testAppendTail items = withTmpSqroll $ \sqroll -> do
    mapM_ (sqrollAppend sqroll) items
    stmt <- makeSelectStatement sqroll Nothing
    items' <- sqrollGetList stmt
    items @=? items'

testSelectEntity :: Assertion
testSelectEntity = withTmpSqroll $ \sqroll -> do
    let user = User "John" "Doe" 32 "kittens"
    key <- sqrollAppend sqroll user
    stmt <- sqrollSelectEntity `fmap` makeSelectStatement sqroll Nothing
    (Entity userId userVal) <- sqrollGetOne stmt
    key @=? userId
    user @=? userVal

testSelectNext :: Assertion
testSelectNext = withTmpSqroll $ \sqroll -> do
    let user1 = User "John" "Doe" 32 "kittens"
        user2 = User "John2" "Doe" 32 "kittens"
    sqrollAppend_ sqroll user1
    sqrollAppend_ sqroll user2
    stmt <- makeSelectStatement sqroll Nothing
    sqrollSelectFromRowId stmt 2
    user <- sqrollGetOne stmt
    user2 @=? user

testFoldAll :: Assertion
testFoldAll = withTmpSqroll $ \sqroll -> do
    let users = [User "John" "Doe" num "kittens" | num <- [1..10]]
    mapM_ (sqrollAppend_ sqroll) users
    stmt <- makeSelectStatement sqroll Nothing
    r <- sqrollFoldAll (\s (User _ _ n _) -> return $ s + n) 0 stmt
    sum [1..10] @=? r


testFold :: Assertion
testFold = withTmpSqroll $ \sqroll -> do
    let users = [User "John" "Doe" num "kittens" | num <- [1..10]]
        cond x = x < 5
    mapM_ (sqrollAppend_ sqroll) users
    stmt <- makeSelectStatement sqroll Nothing
    r <- sqrollFold (\s (User _ _ n _) -> return (s + n, cond n)) 0 stmt
    sum [1..5] @=? r

testListFields :: Assertion
testListFields = withTmpSqroll $ \sqroll -> do
    let bacons = [ SandwichComponent "Bacon" 10
                 , SandwichComponent "Moar bacon!" 100
                 , SandwichComponent "Lots of bacon!!!!" 1000
                 , SandwichComponent "Tiny bun" 1
                 , SandwichComponent "Bacon wraps" 100000
                 ]
        sandwich = Sandwich "Big" "Omnomnomnom" bacons

    _key <- sqrollAppend sqroll sandwich
    key <- sqrollAppend sqroll sandwich
    mapM_ (sqrollAppend_ sqroll) (map (key,) (sandwichComponents sandwich))

    stmt1 <- sqrollSelectEntity `fmap` makeSelectStatement sqroll Nothing
    (Entity key' sandwich') <- last `fmap` sqrollGetList stmt1
    stmt2 <- makeSelectByKeyStatement sqroll Nothing key'
    bacons' <- sqrollGetList stmt2
    let sandwich'' = sandwich' { sandwichComponents = map snd (bacons' :: [(Key Sandwich, SandwichComponent)]) }

    sandwich @=? sandwich''

{-
testCustomQuery :: Assertion
testCustomQuery = withTmpSqroll $ \sqroll -> do
    let users1 = [User "John" "Doe" num "kittens" | num <- [1..10]]
        users2 = [User "Jane" "Doe" num "kittens" | num <- [1..10]]
        users = users1 ++ users2
        expected = filter ((== "John") . userFirstName) . filter ((>= 5) . userAge) $ users

    mapM_ (sqrollAppend_ sqroll) users

    stmt <- makeFlexibleQuery sqroll $ do
        t <- from
        where_ $ ((t ^. UserFirstName) ==. "John") &&. ((t ^. UserAge) >=. 5)
        return t

    users' <- sqrollGetList stmt
    expected @=? users'

testCustomQueryNT :: Assertion
testCustomQueryNT = withTmpSqroll $ \sqroll -> do
    let dogs = [Dog $ Kitten (Just . T.pack $ "foo" ++ show n) | n <- [1..10 :: Int]]
        cats = [Kitten (Just . T.pack $ "bar" ++ show n) | n <- [1..10 :: Int]]

    mapM_ (sqrollAppend_ sqroll) dogs
    mapM_ (sqrollAppend_ sqroll) cats

    let condD = DogKittenWoof ==. Just "foo10"
        condC = KittenWoof ==. Just "bar1"

    dog' <- makeFlexibleQuery sqroll >>= sqrollGetList
    cat' <- makeFlexibleQuery sqroll >>= sqrollGetList

    ([last dogs], [head cats]) @=? (dog', cat')
    -}
