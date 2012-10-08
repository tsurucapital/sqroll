import Test.Framework (defaultMain)

import qualified Database.Sqroll.Tests

main :: IO ()
main = defaultMain
    [ Database.Sqroll.Tests.tests
    ]
