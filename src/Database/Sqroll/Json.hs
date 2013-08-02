module Database.Sqroll.Json
    ( makeSelectBlob
    , makeSelectBlobCond
    )
    where

import Control.Applicative
import Database.Sqroll.Internal
import Database.Sqroll.Sqlite3
import Data.Text (Text)
import Data.List (intercalate)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T


makeSelectBlob :: Sqroll -> String -> IO (Stmt Value Value)
makeSelectBlob s t = makeSelectBlobCond s t Nothing

makeSelectBlobCond :: Sqroll -> String -> Maybe String -> IO (Stmt Value Value)
makeSelectBlobCond sqroll blobTable m'cond = do
        let sql = sqrollSql sqroll
        columns <- sqlTableColumns sql blobTable
        let selectItems = "[" ++ (intercalate "], [" columns) ++ "]"
            cond = maybe "" (" WHERE " ++) m'cond
            request = "SELECT " ++ selectItems ++ " FROM [" ++ blobTable ++ "]" ++ cond
        stmt  <- sqlPrepare sql request
        return (Stmt (stmt, peekBlob (map T.pack columns)))
    where
        peekBlob :: [Text] -> SqlStmt -> IO (Maybe Value)
        peekBlob cols stmt = do
                hasData <- sqlStep stmt
                if hasData
                    then (Just . object) <$> mapM peekBlobField (zip cols [0..])
                    else do sqlReset stmt
                            return Nothing
            where
                peekBlobField :: (Text, Int) -> IO Pair
                peekBlobField (colName, colNum) = do
                    colType <- sqlColumnType stmt colNum
                    case colType of
                        IntColumn   -> ((.=) colName) <$> sqlColumnInt64      stmt colNum
                        FloatColumn -> ((.=) colName) <$> sqlColumnDouble     stmt colNum
                        TextColumn  -> ((.=) colName) <$> sqlColumnText       stmt colNum
                        BlobColumn  -> ((.=) colName) <$> sqlColumnByteString stmt colNum
                        NullColumn  -> return (colName .= ())
