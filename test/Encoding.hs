module Encoding (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Aeson
import Data.Aeson.QQ
import Insomnia.Types

tests :: TestTree
tests = testGroup "Encoding"
  [ testGroup "Workspace"
      [testDefaultWorkspace, testCustomWorkspace]
  , testGroup "Environment"
      [testDefaultEnvironment]
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
  toJSON defaultWorkspace @?= [aesonQQ|
    {
      "_id": "Workspace_Servant-Insomnia",
      "_type": "workspace",
      "name": "Servant-Insomnia",
      "description": "",
      "parentId": null
    }|]

testCustomWorkspace :: TestTree
testCustomWorkspace = testCase "custom" $
  toJSON (createWorkspace "Name" "Description") @?= [aesonQQ|
    {
      "_id": "Workspace_Name",
      "_type": "workspace",
      "name": "Name",
      "description": "Description",
      "parentId": null
    }|]

testDefaultEnvironment :: TestTree
testDefaultEnvironment = testCase "default" $
  toJSON (createEnvironment defaultWorkspace) @?= [aesonQQ|
    {
      "_id": "Environment_of_Workspace_Servant-Insomnia",
      "_type": "environment",
      "data": {
        "baseUrl": "http://localhost:8080"
      }
    }|]
