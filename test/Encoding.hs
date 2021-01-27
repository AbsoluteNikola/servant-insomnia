module Encoding (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Aeson
import Insomnia.Types

tests :: TestTree
tests = testGroup "Encoding"
  [ testGroup "Workspace"
      [testDefaultWorkspace, testCustomWorkspace]
  ]

{- Copy it from my insomnia export file
  "_id": "wrk_761a347d19e74301a2b17a32081d1d61",
  "_type": "workspace",
  "created": 1605687254805,
  "description": "",
  "modified": 1605687254805,
  "name": "Cool name",
  "parentId": null,
-}

testDefaultWorkspace :: TestTree
testDefaultWorkspace = testCase "default" $
  toJSON defaultWorkspace @?= object -- FIXME: change on aeson QQ
    [ "_id" .= String "Workspace_Servant-Insomnia"
    , "_type" .= String "workspace"
    , "name" .= String "Servant-Insomnia"
    , "description" .= String ""
    , "parentId" .= Null
    ]

testCustomWorkspace :: TestTree
testCustomWorkspace = testCase "custom" $
  toJSON (createWorkspace "Name" "Description") @?= object
    [ "_id" .= String "Workspace_Name"
    , "_type" .= String "workspace"
    , "name" .= String "Name"
    , "description" .= String "Description"
    , "parentId" .= Null
    ]
