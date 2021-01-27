module Encoding (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Encoding"
  [ testCase "aaa" $ 1 @?= 1
  ]
