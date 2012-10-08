module Database.Sqroll.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Database.Sqroll
import Database.Sqroll.Tests.Types

tests :: Test
tests = testGroup "Database.Sqroll.Tests"
    [ testCase "testAppendTailUsers" testAppendTailUsers
    ]

testAppendTailUsers :: Assertion
testAppendTailUsers = withTmpScroll $ \sqroll -> do
    ref    <- newIORef []
    tail'  <- sqrollTail sqroll Nothing (\u -> modifyIORef ref (u :))
    append <- sqrollAppend sqroll Nothing

    mapM_ append testUsers
    tail'

    testUsers' <- reverse <$> readIORef ref
    testUsers @=? testUsers'

withTmpScroll :: (Sqroll -> IO a) -> IO a
withTmpScroll f = do
    (tmpPath, sqroll) <- sqrollOpenTmp
    x                 <- f sqroll
    sqrollClose sqroll
    removeFile tmpPath
    return x

sqrollOpenTmp :: IO (FilePath, Sqroll)
sqrollOpenTmp = do
    tmpDir <- getTemporaryDirectory
    let tmpPath = tmpDir </> "sqroll-test.db"
    sqroll <- sqrollOpen tmpPath
    return (tmpPath, sqroll)
