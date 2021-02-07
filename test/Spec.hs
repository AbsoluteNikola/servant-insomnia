import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Encoding
import qualified API

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ Encoding.tests
  , API.tests
  ]
