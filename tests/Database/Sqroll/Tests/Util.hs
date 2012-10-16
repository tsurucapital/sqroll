-- | Sqroll testing utilities
module Database.Sqroll.Tests.Util
    ( withTmpScroll
    , sqrollOpenTmp
    ) where

import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Database.Sqroll

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
