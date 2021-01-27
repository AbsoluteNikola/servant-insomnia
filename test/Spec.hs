import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Encoding

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ Encoding.tests
  ]
