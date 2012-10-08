import Test.Framework (defaultMain)

import qualified Database.Sqroll.Table.Naming.Tests
import qualified Database.Sqroll.Tests

main :: IO ()
main = defaultMain
    [ Database.Sqroll.Table.Naming.Tests.tests
    , Database.Sqroll.Tests.tests
    ]
