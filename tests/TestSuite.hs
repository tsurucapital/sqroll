import Test.Framework (defaultMain)

import qualified Database.Sqroll.Pure.Tests
import qualified Database.Sqroll.Sqlite3.Tests
import qualified Database.Sqroll.Table.Naming.Tests
import qualified Database.Sqroll.Tests

main :: IO ()
main = defaultMain
    [ Database.Sqroll.Pure.Tests.tests
    , Database.Sqroll.Sqlite3.Tests.tests
    , Database.Sqroll.Table.Naming.Tests.tests
    , Database.Sqroll.Tests.tests
    ]
