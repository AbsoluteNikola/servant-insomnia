import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Encoding
import qualified API
import qualified TH

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ Encoding.tests
  , API.tests
  , TH.tests
  ]
