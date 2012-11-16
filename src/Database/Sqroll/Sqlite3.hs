{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Database.Sqroll.Sqlite3
    ( Sql
    , SqlStmt
    , SqlFStmt
    , SqlStatus
    , SqlRowId

    , SqlType (..)
    , sqlTypeToString

    , SqlOpenFlag (..)
    , sqlDefaultOpenFlags

    , sqlOpen
    , sqlClose
    , sqlCheckpoint

    , sqlPrepare
    , sqlStep
    , sqlStepAll
    , sqlStepList
    , sqlStep_
    , sqlReset
    , sqlExecute
    , sqlAllStatements

    , sqlBindInt64
    , sqlBindDouble
    , sqlBindString
    , sqlBindByteString
    , sqlBindLazyByteString
    , sqlBindText
    , sqlBindLazyText
    , sqlBindNothing

    , sqlColumnInt64
    , sqlColumnDouble
    , sqlColumnString
    , sqlColumnByteString
    , sqlColumnLazyByteString
    , sqlColumnText
    , sqlColumnLazyText
    , sqlColumnIsNothing

    , sqlLastInsertRowId
    , sqlGetRowId

    , sqlTableColumns
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Mem (performGC)

-- | Pointer to the db handle itself, can be used as is
type Sql = Ptr ()

-- | Pointer to the prepared sql statement - should be used anywhere outside, contains
-- statement finalizer
type SqlFStmt = ForeignPtr ()

-- | Pointer to the prepared sql statement without finalizer attached, code for binding
-- and picking from statements should use it because of less overhead
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

data SqliteCheckpoint
    = Passive
    | Full
    | Restart
    deriving (Show, Eq)

sqliteCheckpoint :: SqliteCheckpoint -> CInt
sqliteCheckpoint Passive = 0
sqliteCheckpoint Full = 1
sqliteCheckpoint Restart = 2

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

foreign import ccall unsafe "sqlite3.h sqlite3_open_v2" sqlite3_open_v2
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

foreign import ccall unsafe "sqlite3.h sqlite3_close" sqlite3_close
    :: Sql -> IO SqlStatus

sqlClose :: Sql -> IO ()
sqlClose db = do
    performGC
    sqlite3_close db >>= orDie "sqlite3_close"
{-# INLINE sqlClose #-}

foreign import ccall "sqlite3.h sqlite3_prepare_v2" sqlite3_prepare_v2
    :: Sql -> CString -> CInt -> Ptr SqlStmt -> Ptr CString -> IO SqlStatus

sqlPrepare :: Sql -> String -> IO SqlFStmt
sqlPrepare db str = alloca $ \stmtPtr -> withCStringLen str $ \(cstr, len) -> do
    sqlite3_prepare_v2 db cstr (fromIntegral len) stmtPtr nullPtr >>=
        orDie "sqlite3_prepare_v2"
    peek stmtPtr >>= newForeignPtr sqlite3_finalize_ptr
{-# INLINE sqlPrepare #-}

foreign import ccall "sqlite3.h sqlite3_wal_checkpoint_v2" sqlite3_wal_checkpoint_v2
    :: Sql -> Ptr () -> CInt -> Ptr () -> Ptr () -> IO SqlStatus

sqlCheckpoint :: Sql -> IO ()
sqlCheckpoint db = sqlite3_wal_checkpoint_v2 db nullPtr (sqliteCheckpoint Full) nullPtr nullPtr
        >>= orDie "sqlite3_wal_checkpoint_v2"

foreign import ccall unsafe "sqlite3.h sqlite3_step" sqlite3_step
    :: SqlStmt -> IO SqlStatus

sqlStep :: SqlStmt -> IO Bool
sqlStep stmt = sqlite3_step stmt >>= checkStatus
  where
    checkStatus 100 = return True
    checkStatus 101 = return False
    checkStatus s   = error $ "sqlite3_step: status " ++ show s
    {-# INLINE checkStatus #-}
{-# INLINE sqlStep #-}

sqlStepAll :: SqlStmt -> IO () -> IO ()
sqlStepAll stmt f = sqlStep stmt >>= go
  where
    go False = return ()
    go True  = do
        f
        n <- sqlStep stmt
        go n
{-# INLINE sqlStepAll #-}

sqlStepList :: SqlStmt -> IO a -> IO [a]
sqlStepList stmt f = do
    ref <- newIORef []
    sqlStepAll stmt (f >>= \x -> modifyIORef ref (x :))
    reverse <$> readIORef ref

sqlStep_ :: SqlStmt -> IO ()
sqlStep_ stmt = sqlStep stmt >> return ()
{-# INLINE sqlStep_ #-}

foreign import ccall unsafe "sqlite3.h sqlite3_reset" sqlite3_reset
    :: SqlStmt -> IO SqlStatus

sqlReset :: SqlStmt -> IO ()
sqlReset stmt = sqlite3_reset stmt >>= orDie "sqlite3_reset"
{-# INLINE sqlReset #-}

sqlExecute :: Sql -> String -> IO ()
sqlExecute db str = sqlPrepare db str >>= \stmt -> withForeignPtr stmt sqlStep_
{-# INLINE sqlExecute #-}


foreign import ccall unsafe "sqlite3.h sqlite3_next_stmt" sqlite3_next_stmt
    :: Sql -> SqlStmt -> IO SqlStmt

-- | fetch all available prepared sqlite statements
sqlAllStatements :: Sql -> IO [SqlStmt]
sqlAllStatements db = allStmtsR [] nullPtr
    where
        allStmtsR :: [SqlStmt] -> SqlStmt -> IO [SqlStmt]
        allStmtsR acc prev = do
            newStmt <- sqlite3_next_stmt db prev
            if newStmt == nullPtr
                then return acc
                else allStmtsR (newStmt : acc) newStmt

foreign import ccall "sqlite3.h sqlite3_last_insert_rowid"
    sqlite3_last_insert_rowid
    :: Sql -> IO CLLong

foreign import ccall unsafe "sqlite3.h sqlite3_bind_int64" sqlite3_bind_int64
    :: SqlStmt -> CInt -> CLLong -> IO SqlStatus

sqlBindInt64 :: SqlStmt -> Int -> Int64 -> IO ()
sqlBindInt64 stmt n x =
    sqlite3_bind_int64 stmt (fromIntegral n) (fromIntegral x) >>=
        orDie "sqlite3_bind_int64"
{-# INLINE sqlBindInt64 #-}

foreign import ccall unsafe "sqlite3.h sqlite3_bind_double" sqlite3_bind_double
    :: SqlStmt -> CInt -> CDouble -> IO SqlStatus

sqlBindDouble :: SqlStmt -> Int -> Double -> IO ()
sqlBindDouble stmt n x =
    sqlite3_bind_double stmt (fromIntegral n) (realToFrac x) >>=
        orDie "sqlite3_bind_double"
{-# INLINE sqlBindDouble #-}

foreign import ccall unsafe "sqlite3.h sqlite3_bind_text" sqlite3_bind_text
    :: SqlStmt -> CInt -> CString -> CInt -> Ptr () -> IO SqlStatus

sqlBindString :: SqlStmt -> Int -> String -> IO ()
sqlBindString stmt n string = withCStringLen string $ \(cstr, len) ->
    -- The nullPtr we pass here is the destructor for the text we bind. We use
    -- a nullPtr since Haskell will take care of the free'ing (through
    -- withCStringLen).
    sqlite3_bind_text stmt (fromIntegral n) cstr (fromIntegral len) nullPtr >>=
        orDie "sqlite3_bind_text"
{-# INLINE sqlBindString #-}

foreign import ccall unsafe "sqlite3.h sqlite3_bind_blob" sqlite3_bind_blob
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

sqlBindText :: SqlStmt -> Int -> Text -> IO ()
sqlBindText stmt n text = withForeignPtr fptr $ \ptr ->
    sqlite3_bind_text
        stmt (fromIntegral n) (ptr `plusPtr` o) (fromIntegral l) nullPtr >>=
            orDie "sqlite3_bind_text"
  where
    bs           = T.encodeUtf8 text
    (fptr, o, l) = BI.toForeignPtr bs
{-# INLINE sqlBindText #-}

sqlBindLazyText :: SqlStmt -> Int -> TL.Text -> IO ()
sqlBindLazyText stmt n = sqlBindText stmt n . TL.toStrict
{-# INLINE sqlBindLazyText #-}

foreign import ccall unsafe "sqlite3.h sqlite3_bind_null" sqlite3_bind_null
    :: SqlStmt -> CInt -> IO SqlStatus

sqlBindNothing :: SqlStmt -> Int -> IO ()
sqlBindNothing stmt n = sqlite3_bind_null stmt (fromIntegral n) >>=
    orDie "sqlite3_bind_null"
{-# INLINE sqlBindNothing #-}

foreign import ccall unsafe "sqlite3.h sqlite3_column_int64" sqlite3_column_int64
    :: SqlStmt -> CInt -> IO CLLong

sqlColumnInt64 :: SqlStmt -> Int -> IO Int64
sqlColumnInt64 stmt n =
    fmap fromIntegral $ sqlite3_column_int64 stmt (fromIntegral n)
{-# INLINE sqlColumnInt64 #-}

foreign import ccall unsafe "sqlite3.h sqlite3_column_double" sqlite3_column_double
    :: SqlStmt -> CInt -> IO CDouble

sqlColumnDouble :: SqlStmt -> Int -> IO Double
sqlColumnDouble stmt n =
    fmap realToFrac $ sqlite3_column_double stmt (fromIntegral n)
{-# INLINE sqlColumnDouble #-}

foreign import ccall unsafe "sqlite3.h sqlite3_column_text" sqlite3_column_text
    :: SqlStmt -> CInt -> IO CString

sqlColumnString :: SqlStmt -> Int -> IO String
sqlColumnString stmt n =
    sqlite3_column_text stmt (fromIntegral n) >>= peekCString
{-# INLINE sqlColumnString #-}

foreign import ccall unsafe "sqlite3.h sqlite3_column_blob" sqlite3_column_blob
    :: SqlStmt -> CInt -> IO (Ptr ())

foreign import ccall unsafe "sqlite3.h sqlite3_column_bytes" sqlite3_column_bytes
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

sqlColumnText :: SqlStmt -> Int -> IO Text
sqlColumnText stmt =
    -- See sqlite3 conversion table: this should be perfectly safe
    fmap T.decodeUtf8 . sqlColumnByteString stmt
{-# INLINE sqlColumnText #-}

sqlColumnLazyText :: SqlStmt -> Int -> IO TL.Text
sqlColumnLazyText stmt = fmap TL.fromStrict . sqlColumnText stmt
{-# INLINE sqlColumnLazyText #-}

foreign import ccall unsafe "sqlite3.h sqlite3_column_type" sqlite3_column_type
    :: SqlStmt -> CInt -> IO CInt

sqlColumnIsNothing :: SqlStmt -> Int -> IO Bool
sqlColumnIsNothing stmt n = do
    t <- sqlite3_column_type stmt (fromIntegral n)
    return $ t == 5  -- SQLITE_NULL
{-# INLINE sqlColumnIsNothing #-}

foreign import ccall "sqlite3.h &sqlite3_finalize" sqlite3_finalize_ptr
    :: FunPtr (SqlStmt -> IO ())

sqlLastInsertRowId :: Sql -> IO SqlRowId
sqlLastInsertRowId = fmap fromIntegral . sqlite3_last_insert_rowid
{-# INLINE sqlLastInsertRowId #-}

sqlGetRowId :: SqlStmt -> IO SqlRowId
sqlGetRowId stmt = sqlColumnInt64 stmt 0
{-# INLINE sqlGetRowId #-}

-- | Get all the column names for a given table
sqlTableColumns :: Sql -> String -> IO [String]
sqlTableColumns sql tableName = do
    stmt <- sqlPrepare sql $ "PRAGMA table_info(" ++ tableName ++ ")"
    cols <- withForeignPtr stmt $ \raw -> sqlStepList raw $ sqlColumnString raw 1
    return cols

orDie :: String -> SqlStatus -> IO ()
orDie _   0 = return ()
orDie msg s = error $ msg ++ ": " ++ showStatus s
{-# INLINE orDie #-}

showStatus :: SqlStatus -> String
showStatus 0 = "Successful result"
showStatus 1 = "SQL error or missing database"
showStatus 2 = "Internal logic error in SQLite"
showStatus 3 = "Access permission denied"
showStatus 4 = "Callback routine requested an abort"
showStatus 5 = "The database file is locked"
showStatus 6 = "A table in the database is locked"
showStatus 7 = "A malloc() failed"
showStatus 8 = "Attempt to write a readonly database"
showStatus 9 = "Operation terminated by sqlite3_interrupt()"
showStatus 10 = "Some kind of disk I/O error occurred"
showStatus 11 = "The database disk image is malformed"
showStatus 12 = "Unknown opcode in sqlite3_file_control()"
showStatus 13 = "Insertion failed because database is full"
showStatus 14 = "Unable to open the database file"
showStatus 15 = "Database lock protocol error"
showStatus 16 = "Database is empty"
showStatus 17 = "The database schema changed"
showStatus 18 = "String or BLOB exceeds size limit"
showStatus 19 = "Abort due to constraint violation"
showStatus 20 = "Data type mismatch"
showStatus 21 = "Library used incorrectly"
showStatus 22 = "Uses OS features not supported on host"
showStatus 23 = "Authorization denied"
showStatus 24 = "Auxiliary database format error"
showStatus 25 = "2nd parameter to sqlite3_bind out of range"
showStatus 26 = "File opened that is not a database file"
showStatus 100 = "sqlite3_step() has another row ready"
showStatus 101 = "sqlite3_step() has finished executing"
showStatus _ = "Unknown error code"
