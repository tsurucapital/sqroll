{-# LANGUAGE ForeignFunctionInterface #-}
-- | Sqroll testing utilities
module Database.Sqroll.Tests.Util
    ( withTmpFile
    , withTmpSqroll
    ) where

import Control.Exception (bracket)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Database.Sqroll

withTmpFile :: (FilePath -> IO a) -> IO a
withTmpFile = bracket getTmpFile removeFile


getTmpFile :: IO FilePath
getTmpFile = do
    tmpDir <- getTemporaryDirectory
    pid    <- c_getpid
    return $ tmpDir </> "sqroll-test-" ++ show pid ++ ".db"

foreign import ccall "getpid" c_getpid :: IO Int

withTmpSqroll :: (Sqroll -> IO a) -> IO a
withTmpSqroll f = withTmpFile $ \filename -> bracket (sqrollOpen filename) sqrollClose f
