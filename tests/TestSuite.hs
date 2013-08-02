import Test.Framework (defaultMain)

import qualified Database.Sqroll.Pure.Tests
import qualified Database.Sqroll.Sqlite3.Tests
import qualified Database.Sqroll.Table.Naming.Tests
import qualified Database.Sqroll.TH.Tests
import qualified Database.Sqroll.Json.Tests
import qualified Database.Sqroll.Tests
import qualified Database.Sqroll.Flexible.Tests

main :: IO ()
main = defaultMain
    [ Database.Sqroll.Pure.Tests.tests
    , Database.Sqroll.Sqlite3.Tests.tests
    , Database.Sqroll.Table.Naming.Tests.tests
    , Database.Sqroll.TH.Tests.tests
    , Database.Sqroll.Tests.tests
    , Database.Sqroll.Flexible.Tests.tests
    , Database.Sqroll.Json.Tests.tests
    ]
