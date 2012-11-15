module Database.Sqroll
    ( NamedTable (..)

    , HasTable (..)
    , HString (..)
    , aliasTable

    , Key (..)
    , Entity (..)

    , Sqroll (sqrollSql)
    , sqrollOpen
    , sqrollOpenWith
    , sqrollClose
    , sqrollCheckpoint

    , sqrollTransaction

    , sqrollAppend
    , sqrollAppend_

    , makeSelectStatement
    , makeSelectByKeyStatement

    , sqrollSelectEntity
    , sqrollSelectFromRowId

    , sqrollGetList
    , sqrollGetLazyList
    , sqrollFold
    , sqrollFoldAll
    , sqrollGetOne
    , sqrollGetMaybe
    ) where

import Database.Sqroll.Internal
import Database.Sqroll.Table.Field (HString (..))
