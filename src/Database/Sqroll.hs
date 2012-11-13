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
    , sqrollFinalize

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
    ) where

import Database.Sqroll.Internal
import Database.Sqroll.Table.Field (HString (..))
