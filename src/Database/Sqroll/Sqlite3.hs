{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Database.Sqroll.Sqlite3
    ( Sql
    , SqlStmt
    , SqlStatus
    , SqlRowId

    , SqlType (..)
    , sqlTypeToString

    , SqlOpenFlag (..)
    , sqlDefaultOpenFlags
    
    , sqlOpen
    , sqlClose

    , sqlPrepare
    , sqlFinalize
    , sqlStep
    , sqlStepAll
    , sqlStep_
    , sqlReset
    , sqlExecute

    , sqlBindInt64
    , sqlBindDouble
    , sqlBindString
    , sqlBindByteString
    , sqlBindLazyByteString
    , sqlBindNothing
    
    , sqlColumnInt64
    , sqlColumnDouble
    , sqlColumnString
    , sqlColumnByteString
    , sqlColumnLazyByteString
    , sqlColumnIsNothing

    , sqlLastInsertRowId
    , sqlLastSelectRowid

    , sqlTableColumns
    ) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (foldl')
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

type Sql = Ptr ()
type SqlStmt = Ptr ()
type SqlStatus = CInt

type SqlRowId = Int64

data SqlType
    = SqlInteger
    | SqlText
    | SqlDouble
    | SqlBlob
    deriving (Show, Eq)

sqlTypeToString :: SqlType -> String
sqlTypeToString SqlInteger = "INTEGER"
sqlTypeToString SqlText    = "TEXT"
sqlTypeToString SqlDouble  = "DOUBLE"
sqlTypeToString SqlBlob    = "BLOB"

data SqlOpenFlag
    = SqlOpenReadOnly
    | SqlOpenReadWrite
    | SqlOpenCreate
    | SqlOpenWal
    deriving (Eq, Show)

sqlDefaultOpenFlags :: [SqlOpenFlag]
sqlDefaultOpenFlags = [SqlOpenReadWrite, SqlOpenCreate, SqlOpenWal]

sqlOpenFlagCode :: SqlOpenFlag -> CInt
sqlOpenFlagCode SqlOpenReadOnly  = 0x00000001
sqlOpenFlagCode SqlOpenReadWrite = 0x00000002
sqlOpenFlagCode SqlOpenCreate    = 0x00000004
sqlOpenFlagCode SqlOpenWal       = 0x00000000

foreign import ccall "sqlite3.h sqlite3_open_v2" sqlite3_open_v2
    :: CString -> Ptr Sql -> CInt -> CString -> IO SqlStatus

sqlOpen :: FilePath -> [SqlOpenFlag] -> IO Sql
sqlOpen fp flags = do
    sql <- alloca $ \db -> withCString fp $ \cfp -> do
        sqlite3_open_v2 cfp db flag nullPtr >>= orDie "sqlite3_open"
        peek db

    when (SqlOpenWal `elem` flags) $
        sqlExecute sql "PRAGMA journal_mode=WAL;"

    return sql
  where
    flag = foldl' (.|.) 0 $ map sqlOpenFlagCode flags
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

sqlExecute :: Sql -> String -> IO ()
sqlExecute db str = bracket (sqlPrepare db str) sqlFinalize sqlStep >> return ()
{-# INLINE sqlExecute #-}

foreign import ccall "sqlite3.h sqlite3_last_insert_rowid"
    sqlite3_last_insert_rowid
    :: Sql -> IO CLLong

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

foreign import ccall "sqlite3.h sqlite3_bind_blob" sqlite3_bind_blob
    :: SqlStmt -> CInt -> Ptr () -> CInt -> Ptr () -> IO SqlStatus

sqlBindByteString :: SqlStmt -> Int -> ByteString -> IO ()
sqlBindByteString stmt n bs = withForeignPtr fptr $ \ptr ->
    sqlite3_bind_blob
        stmt (fromIntegral n) (ptr `plusPtr` o) (fromIntegral l) nullPtr >>=
            orDie "sqlite3_bind_blob"
  where
    (fptr, o, l) = BI.toForeignPtr bs
{-# INLINE sqlBindByteString #-}

sqlBindLazyByteString :: SqlStmt -> Int -> BL.ByteString -> IO ()
sqlBindLazyByteString stmt n lbs = sqlBindByteString stmt n $
    B.concat $ BL.toChunks lbs
{-# INLINE sqlBindLazyByteString #-}

foreign import ccall "sqlite3.h sqlite3_bind_null" sqlite3_bind_null
    :: SqlStmt -> CInt -> IO SqlStatus

sqlBindNothing :: SqlStmt -> Int -> IO ()
sqlBindNothing stmt n = sqlite3_bind_null stmt (fromIntegral n) >>=
    orDie "sqlite3_bind_null"
{-# INLINE sqlBindNothing #-}

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

foreign import ccall "sqlite3.h sqlite3_column_blob" sqlite3_column_blob
    :: SqlStmt -> CInt -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_column_bytes" sqlite3_column_bytes
    :: SqlStmt -> CInt -> IO CInt

sqlColumnByteString :: SqlStmt -> Int -> IO ByteString
sqlColumnByteString stmt n = do
    bytes <- fromIntegral <$> sqlite3_column_bytes stmt n'
    fptr  <- mallocForeignPtrBytes bytes
    sqlp  <- sqlite3_column_blob stmt n'

    withForeignPtr fptr $ \ptr ->
        BI.memcpy ptr (castPtr sqlp) (fromIntegral bytes)

    return $ BI.fromForeignPtr fptr 0 bytes
  where
    n' = fromIntegral n
{-# INLINE sqlColumnByteString #-}

sqlColumnLazyByteString :: SqlStmt -> Int -> IO BL.ByteString
sqlColumnLazyByteString stmt n = fmap (BL.fromChunks . return) $
    sqlColumnByteString stmt n
{-# INLINE sqlColumnLazyByteString #-}

foreign import ccall "sqlite3.h sqlite3_column_type" sqlite3_column_type
    :: SqlStmt -> CInt -> IO CInt

sqlColumnIsNothing :: SqlStmt -> Int -> IO Bool
sqlColumnIsNothing stmt n = do
    t <- sqlite3_column_type stmt (fromIntegral n)
    return $ t == 5  -- SQLITE_NULL
{-# INLINE sqlColumnIsNothing #-}

foreign import ccall "sqlite3.h sqlite3_finalize" sqlite3_finalize
    :: SqlStmt -> IO SqlStatus

sqlLastInsertRowId :: Sql -> IO SqlRowId
sqlLastInsertRowId = fmap fromIntegral . sqlite3_last_insert_rowid
{-# INLINE sqlLastInsertRowId #-}

sqlLastSelectRowid :: SqlStmt -> IO SqlRowId
sqlLastSelectRowid stmt = sqlColumnInt64 stmt 0
{-# INLINE sqlLastSelectRowid #-}

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
