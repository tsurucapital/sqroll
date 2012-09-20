{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Database.Sqroll.Sqlite3 where

import Control.Exception (bracket)
import Data.Bits ((.|.))
import Data.Int (Int64)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

type Sql = Ptr ()
type SqlStmt = Ptr ()
type SqlStatus = CInt

newtype SqlRowId = SqlRowId Int64
    deriving (Show)

type ForeignKey a = SqlRowId

foreign import ccall "sqlite3.h sqlite3_open_v2" sqlite3_open_v2
    :: CString -> Ptr Sql -> CInt -> CString -> IO SqlStatus

sqlOpen :: FilePath -> IO Sql
sqlOpen fp = alloca $ \db -> withCString fp $ \cfp -> do
    sqlite3_open_v2 cfp db flags nullPtr >>= orDie "sqlite3_open"
    peek db
  where
    flags = 0x00000002  -- SQL_OPEN_READWRITE
        .|. 0x00000004  -- SQLITE_OPEN_CREATE
{-# INLINE sqlOpen #-}

foreign import ccall "sqlite3.h sqlite3_close" sqlite3_close
    :: Sql -> IO SqlStatus

sqlClose :: Sql -> IO ()
sqlClose db = sqlite3_close db >>= orDie "sqlite3_close"
{-# INLINE sqlClose #-}

foreign import ccall "sqlite3.h sqlite3_prepare_v2" sqlite3_prepare_v2
    :: Sql -> CString -> CInt -> Ptr SqlStmt -> Ptr CString -> IO SqlStatus

sqlPrepare :: Sql -> String -> IO SqlStmt
sqlPrepare db str = alloca $ \stmtPtr -> withCString str $ \cstr -> do
    sqlite3_prepare_v2 db cstr (-1) stmtPtr nullPtr >>=
        orDie "sqlite3_prepare_v2"
    peek stmtPtr
{-# INLINE sqlPrepare #-}

foreign import ccall "sqlite3.h sqlite3_bind_int64" sqlite3_bind_int64
    :: SqlStmt -> CInt -> CLLong -> IO SqlStatus

sqlBindInt64 :: SqlStmt -> Int -> Int64 -> IO ()
sqlBindInt64 stmt n x =
    sqlite3_bind_int64 stmt (fromIntegral n) (fromIntegral x) >>=
        orDie "sqlite3_bind_int64"
{-# INLINE sqlBindInt64 #-}

foreign import ccall "sqlite3.h sqlite3_bind_double" sqlite3_bind_double
    :: SqlStmt -> CInt -> CDouble -> IO SqlStatus

sqlBindDouble :: SqlStmt -> Int -> Double -> IO ()
sqlBindDouble stmt n x =
    sqlite3_bind_double stmt (fromIntegral n) (realToFrac x) >>=
        orDie "sqlite3_bind_double"
{-# INLINE sqlBindDouble #-}

foreign import ccall "sqlite3.h sqlite3_bind_text" sqlite3_bind_text
    :: SqlStmt -> CInt -> CString -> CInt -> Ptr () -> IO SqlStatus

sqlBindString :: SqlStmt -> Int -> String -> IO ()
sqlBindString stmt n string = withCStringLen string $ \(cstr, len) ->
    -- The nullPtr we pass here is the destructor for the text we bind. We use
    -- a nullPtr since Haskell will take care of the free'ing (through
    -- withCStringLen).
    sqlite3_bind_text stmt (fromIntegral n) cstr (fromIntegral len) nullPtr >>=
        orDie "sqlite3_bind_text"
{-# INLINE sqlBindString #-}

foreign import ccall "sqlite3.h sqlite3_column_int64" sqlite3_column_int64
    :: SqlStmt -> CInt -> IO CLLong

sqlColumnInt64 :: SqlStmt -> Int -> IO Int64
sqlColumnInt64 stmt n =
    fmap fromIntegral $ sqlite3_column_int64 stmt (fromIntegral n)
{-# INLINE sqlColumnInt64 #-}

foreign import ccall "sqlite3.h sqlite3_column_double" sqlite3_column_double
    :: SqlStmt -> CInt -> IO CDouble

sqlColumnDouble :: SqlStmt -> Int -> IO Double
sqlColumnDouble stmt n =
    fmap realToFrac $ sqlite3_column_double stmt (fromIntegral n)
{-# INLINE sqlColumnDouble #-}

foreign import ccall "sqlite3.h sqlite3_column_text" sqlite3_column_text
    :: SqlStmt -> CInt -> IO CString

sqlColumnString :: SqlStmt -> Int -> IO String
sqlColumnString stmt n =
    sqlite3_column_text stmt (fromIntegral n) >>= peekCString
{-# INLINE sqlColumnString #-}

foreign import ccall "sqlite3.h sqlite3_finalize" sqlite3_finalize
    :: SqlStmt -> IO SqlStatus

sqlFinalize :: SqlStmt -> IO ()
sqlFinalize stmt = sqlite3_finalize stmt >>= orDie "sqlite3_finalize"
{-# INLINE sqlFinalize #-}

foreign import ccall "sqlite3.h sqlite3_step" sqlite3_step
    :: SqlStmt -> IO SqlStatus

sqlStep :: SqlStmt -> IO Bool
sqlStep stmt = sqlite3_step stmt >>= checkStatus
  where
    checkStatus 100 = return True
    checkStatus 101 = return False
    checkStatus s   = error $ "sqlite3_step: status " ++ show s
    {-# INLINE checkStatus #-}
{-# INLINE sqlStep #-}

sqlStepAll :: SqlStmt -> IO a -> IO [a]
sqlStepAll stmt f = sqlStep stmt >>= go
  where
    go False = return []
    go True  = do
        x <- f
        n <- sqlStep stmt
        fmap (x :) (go n)
{-# INLINE sqlStepAll #-}

sqlStep_ :: SqlStmt -> IO ()
sqlStep_ stmt = sqlStep stmt >> return ()
{-# INLINE sqlStep_ #-}

foreign import ccall "sqlite3.h sqlite3_reset" sqlite3_reset
    :: SqlStmt -> IO SqlStatus

sqlReset :: SqlStmt -> IO ()
sqlReset stmt = sqlite3_reset stmt >>= orDie "sqlite3_reset"
{-# INLINE sqlReset #-}

foreign import ccall "sqlite3.h sqlite3_last_insert_rowid"
    sqlite3_last_insert_rowid
    :: Sql -> IO CLLong

sqlLastInsertRowId :: Sql -> IO SqlRowId
sqlLastInsertRowId = fmap (SqlRowId . fromIntegral) . sqlite3_last_insert_rowid
{-# INLINE sqlLastInsertRowId #-}

sqlExecute :: Sql -> String -> IO ()
sqlExecute db str = bracket (sqlPrepare db str) sqlFinalize sqlStep >> return ()
{-# INLINE sqlExecute #-}

-- | Get all the column names for a given table
sqlTableColumns :: Sql -> String -> IO [String]
sqlTableColumns sql tableName = do
    stmt <- sqlPrepare sql $ "PRAGMA table_info(" ++ tableName ++ ")"
    cols <- sqlStepAll stmt $ sqlColumnString stmt 1
    sqlFinalize stmt
    return cols

orDie :: String -> SqlStatus -> IO ()
orDie _   0 = return ()
orDie msg s = error $ msg ++ ": status " ++ show s
{-# INLINE orDie #-}
