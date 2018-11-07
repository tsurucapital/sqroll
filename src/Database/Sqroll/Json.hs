module Database.Sqroll.Json
    ( makeSelectBlob
    , makeSelectBlobCond
    )
    where

import Database.Sqroll.Internal
import Database.Sqroll.Sqlite3
import Data.ByteString.Base64 as Base64
import Data.Text (Text)
import Data.List (intercalate)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


makeSelectBlob :: Sqroll -> Bool -> String -> IO (Stmt Value Value)
makeSelectBlob s v t = makeSelectBlobCond s v t Nothing

makeSelectBlobCond :: Sqroll -> Bool -> String -> Maybe String -> IO (Stmt Value Value)
makeSelectBlobCond sqroll verbose blobTable m'cond = do
        let sql = sqrollSql sqroll
        columns <- sqlTableColumns sql blobTable
        let selectItems = "[" ++ (intercalate "], [" columns) ++ "]"
            cond = maybe "" (" WHERE " ++) m'cond
            request = "SELECT " ++ selectItems ++ " FROM [" ++ blobTable ++ "]" ++ cond
            nc = length columns
        stmt  <- sqlPrepare sql request
        return (Stmt (stmt, peekBlob (map T.pack columns) nc))
    where
        peekBlob :: [Text] -> Int -> SqlStmt -> IO (Maybe Value)
        peekBlob cols nc stmt = do
                hasData <- sqlStep stmt
                if hasData
                    then do vals <- mapM peekBlobField [0 .. nc-1]
                            return $ if verbose
                                then (Just . object) $ zipWith (.=) cols vals
                                else (Just . toJSON) vals
                    else do sqlReset stmt
                            return Nothing
            where
                encByteString = T.decodeLatin1 . Base64.encode

                peekBlobField :: Int -> IO Value
                peekBlobField colNum = do
                    colType <- sqlColumnType stmt colNum
                    case colType of
                        IntColumn   -> toJSON <$> sqlColumnInt64  stmt colNum
                        FloatColumn -> toJSON <$> sqlColumnDouble stmt colNum
                        TextColumn  -> toJSON <$> sqlColumnText   stmt colNum
                        BlobColumn  -> (toJSON . encByteString) <$> sqlColumnByteString stmt colNum
                        NullColumn  -> return Null
