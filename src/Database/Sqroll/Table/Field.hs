{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Sqroll.Table.Field
    ( Field (..)
    , fieldIndexed
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Beamable (Beamable, decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (Monoid, mempty)
import Data.Time (Day (..), UTCTime (..), formatTime, parseTime)
import Data.Int (Int64)
import System.Locale (defaultTimeLocale)

import Database.Sqroll.Sqlite3

class Field a where
    fieldTypes   :: a -> [SqlType]  -- Should work with 'undefined'
    fieldRefers  :: a -> [String]   -- Should work with 'undefined'
    fieldDefault :: a
    fieldPoke    :: SqlStmt -> Int -> a -> IO ()
    fieldPeek    :: SqlStmt -> Int -> IO a

    -- Defaults: use Beamable
    default fieldTypes :: Beamable a => a -> [SqlType]
    fieldTypes = const [SqlBlob]

    default fieldRefers :: a -> [String]
    fieldRefers = const []

    default fieldDefault :: Monoid a => a
    fieldDefault = mempty

    default fieldPoke :: Beamable a => SqlStmt -> Int -> a -> IO ()
    fieldPoke stmt n x = sqlBindByteString stmt n (encode x)

    default fieldPeek :: Beamable a => SqlStmt -> Int -> IO a
    fieldPeek stmt n = fmap decode $ sqlColumnByteString stmt n

instance Field Int where
    fieldTypes   = const [SqlInteger]
    fieldDefault = 0

    fieldPoke stmt n = sqlBindInt64 stmt n . fromIntegral
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap fromIntegral (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field Bool where
    fieldTypes   = const [SqlInteger]
    fieldDefault = False

    fieldPoke stmt n False = sqlBindInt64 stmt n 0
    fieldPoke stmt n True  = sqlBindInt64 stmt n 1
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = fmap (/= 0) (sqlColumnInt64 stmt n)
    {-# INLINE fieldPeek #-}

instance Field Int64 where
    fieldTypes   = const [SqlInteger]
    fieldDefault = 0

    fieldPoke = sqlBindInt64
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnInt64
    {-# INLINE fieldPeek #-}

instance Field String where
    fieldTypes   = const [SqlText]
    fieldDefault = ""

    fieldPoke = sqlBindString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnString
    {-# INLINE fieldPeek #-}

instance Field Double where
    fieldTypes   = const [SqlDouble]
    fieldDefault = 0

    fieldPoke    = sqlBindDouble
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnDouble
    {-# INLINE fieldPeek #-}

instance Field ByteString where
    fieldTypes   = const [SqlBlob]
    fieldDefault = B.empty

    fieldPoke = sqlBindByteString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnByteString
    {-# INLINE fieldPeek #-}

instance Field BL.ByteString where
    fieldTypes   = const [SqlBlob]
    fieldDefault = BL.empty

    fieldPoke = sqlBindLazyByteString
    {-# INLINE fieldPoke #-}

    fieldPeek = sqlColumnLazyByteString
    {-# INLINE fieldPeek #-}

instance forall a. Field a => Field (Maybe a) where
    fieldTypes   = const $ fieldTypes (ud :: a)
    fieldDefault = Nothing

    fieldPoke stmt n Nothing  = sqlBindNothing stmt n
    fieldPoke stmt n (Just x) = fieldPoke stmt n x
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = do
        nothing <- sqlColumnIsNothing stmt n
        if nothing
            then return Nothing
            else fmap Just $ fieldPeek stmt n
    {-# INLINE fieldPeek #-}

instance Field UTCTime where
    fieldTypes   = const [SqlText]
    fieldDefault = UTCTime (ModifiedJulianDay 0) 0

    fieldPoke stmt n time = sqlBindString stmt n (formatSqliteTime time)
    {-# INLINE fieldPoke #-}

    fieldPeek stmt = fmap parseSqliteTime . sqlColumnString stmt
    {-# INLINE fieldPeek #-}

instance forall a b. (Field a, Field b) => Field (a, b) where
    fieldTypes _ = fieldTypes (ud :: a) ++ fieldTypes (ud :: b)
    fieldDefault = (fieldDefault, fieldDefault)

    fieldPoke stmt n (x, y) = do
        fieldPoke stmt n x
        fieldPoke stmt (n + cols x) y
    {-# INLINE fieldPoke #-}

    fieldPeek stmt n = (,)
        <$> fieldPeek stmt n
        <*> fieldPeek stmt (n + cols (ud :: a))
    {-# INLINE fieldPeek #-}

instance forall a b c. (Field a, Field b, Field c) => Field (a, b, c) where
    fieldTypes _ =
        fieldTypes (ud :: a) ++ fieldTypes (ud :: b) ++ fieldTypes (ud :: c)
    fieldDefault = (fieldDefault, fieldDefault, fieldDefault)

    fieldPoke stmt n (x, y, z) = do
        let n'  = n  + cols x
            n'' = n' + cols y
        fieldPoke stmt n   x
        fieldPoke stmt n'  y
        fieldPoke stmt n'' z

    fieldPeek stmt n = do
        let n'  = n  + cols (ud :: a)
            n'' = n' + cols (ud :: b)
        (,,) <$> fieldPeek stmt n <*> fieldPeek stmt n' <*> fieldPeek stmt n''

instance forall a b c d. (Field a, Field b, Field c, Field d) =>
        Field (a, b, c, d) where
    fieldTypes _ = fieldTypes (ud :: a) ++ fieldTypes (ud :: b) ++
        fieldTypes (ud :: c) ++ fieldTypes (ud :: d)
    fieldDefault = (fieldDefault, fieldDefault, fieldDefault, fieldDefault)

    fieldPoke stmt n (x, y, z, u) = do
        let n'   = n   + cols x
            n''  = n'  + cols y
            n''' = n'' + cols z
        fieldPoke stmt n    x
        fieldPoke stmt n'   y
        fieldPoke stmt n''  z
        fieldPoke stmt n''' u

    fieldPeek stmt n = do
        let n'   = n   + cols (ud :: a)
            n''  = n'  + cols (ud :: b)
            n''' = n'' + cols (ud :: c)
        (,,,) <$> fieldPeek stmt n <*> fieldPeek stmt n' <*>
            fieldPeek stmt n'' <*> fieldPeek stmt n'''

instance forall a b c d e. (Field a, Field b, Field c, Field d, Field e) =>
        Field (a, b, c, d, e) where
    fieldTypes _ = fieldTypes (ud :: a) ++ fieldTypes (ud :: b) ++
        fieldTypes (ud :: c) ++ fieldTypes (ud :: d) ++ fieldTypes (ud :: e)
    fieldDefault = (fieldDefault, fieldDefault, fieldDefault, fieldDefault, fieldDefault)

    fieldPoke stmt n0 (f0, f1, f2, f3, f4) = do
        let n1 = n0 + cols f0
            n2 = n1 + cols f1
            n3 = n2 + cols f2
            n4 = n3 + cols f3
        fieldPoke stmt n0 f0
        fieldPoke stmt n1 f1
        fieldPoke stmt n2 f2
        fieldPoke stmt n3 f3
        fieldPoke stmt n4 f4

    fieldPeek stmt n0 = do
        let n1 = n0 + cols (ud :: a)
            n2 = n1 + cols (ud :: b)
            n3 = n2 + cols (ud :: c)
            n4 = n3 + cols (ud :: d)
        (,,,,) <$> fieldPeek stmt n0 <*> fieldPeek stmt n1 <*>
            fieldPeek stmt n2 <*> fieldPeek stmt n3 <*> fieldPeek stmt n4

instance forall a b c d e f. (Field a, Field b, Field c, Field d, Field e, Field f) =>
        Field (a, b, c, d, e, f) where
    fieldTypes _ = fieldTypes (ud :: a) ++ fieldTypes (ud :: b) ++
        fieldTypes (ud :: c) ++ fieldTypes (ud :: d) ++ fieldTypes (ud :: e) ++
        fieldTypes (ud :: f)

    fieldDefault = (fieldDefault, fieldDefault, fieldDefault, fieldDefault,
                    fieldDefault, fieldDefault)

    fieldPoke stmt n0 (f0, f1, f2, f3, f4, f5) = do
        let n1 = n0 + cols f0
            n2 = n1 + cols f1
            n3 = n2 + cols f2
            n4 = n3 + cols f3
            n5 = n4 + cols f4
        fieldPoke stmt n0 f0
        fieldPoke stmt n1 f1
        fieldPoke stmt n2 f2
        fieldPoke stmt n3 f3
        fieldPoke stmt n4 f4
        fieldPoke stmt n5 f5

    fieldPeek stmt n0 = do
        let n1 = n0 + cols (ud :: a)
            n2 = n1 + cols (ud :: b)
            n3 = n2 + cols (ud :: c)
            n4 = n3 + cols (ud :: d)
            n5 = n4 + cols (ud :: f)
        (,,,,,) <$> fieldPeek stmt n0 <*> fieldPeek stmt n1 <*>
            fieldPeek stmt n2 <*> fieldPeek stmt n3 <*> fieldPeek stmt n4 <*>
            fieldPeek stmt n5

instance forall a b c d e f g. (Field a, Field b, Field c, Field d, Field e, Field f, Field g) =>
        Field (a, b, c, d, e, f, g) where
    fieldTypes _ = fieldTypes (ud :: a) ++ fieldTypes (ud :: b) ++ fieldTypes (ud :: c) ++
                   fieldTypes (ud :: d) ++ fieldTypes (ud :: e) ++ fieldTypes (ud :: f) ++
                   fieldTypes (ud :: g)

    fieldDefault = (fieldDefault, fieldDefault, fieldDefault, fieldDefault,
                    fieldDefault, fieldDefault, fieldDefault)

    fieldPoke stmt n0 (f0, f1, f2, f3, f4, f5, f6) = do
        let n1 = n0 + cols f0
            n2 = n1 + cols f1
            n3 = n2 + cols f2
            n4 = n3 + cols f3
            n5 = n4 + cols f4
            n6 = n5 + cols f5
        fieldPoke stmt n0 f0
        fieldPoke stmt n1 f1
        fieldPoke stmt n2 f2
        fieldPoke stmt n3 f3
        fieldPoke stmt n4 f4
        fieldPoke stmt n5 f5
        fieldPoke stmt n6 f6

    fieldPeek stmt n0 = do
        let n1 = n0 + cols (ud :: a)
            n2 = n1 + cols (ud :: b)
            n3 = n2 + cols (ud :: c)
            n4 = n3 + cols (ud :: d)
            n5 = n4 + cols (ud :: f)
            n6 = n5 + cols (ud :: g)
        (,,,,,,) <$> fieldPeek stmt n0 <*> fieldPeek stmt n1 <*>
            fieldPeek stmt n2 <*> fieldPeek stmt n3 <*> fieldPeek stmt n4 <*>
            fieldPeek stmt n5 <*> fieldPeek stmt n6

fieldIndexed :: Field a => a -> Bool
fieldIndexed = not . null . fieldRefers

formatSqliteTime :: UTCTime -> String
formatSqliteTime = take 23 . formatTime defaultTimeLocale sqliteTimeFmt
{-# INLINE formatSqliteTime #-}

parseSqliteTime :: String -> UTCTime
parseSqliteTime string =
    case parseTime defaultTimeLocale sqliteTimeFmt string of
        Just t  -> t
        Nothing -> error $ "parseSqliteTime: Could not parse: " ++ string
{-# INLINE parseSqliteTime #-}

sqliteTimeFmt :: String
sqliteTimeFmt = "%Y-%m-%d %H:%M:%S%Q"
{-# INLINE sqliteTimeFmt #-}

-- | Shorthand...
ud :: a
ud = undefined
{-# INLINE ud #-}

cols :: Field a => a -> Int
cols = length . fieldTypes
{-# INLINE cols #-}
