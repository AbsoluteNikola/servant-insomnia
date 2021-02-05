module Encoding (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Aeson
import Data.Aeson.QQ
import Insomnia.Types
import Network.HTTP.Types ( StdMethod(..) )

tests :: TestTree
tests = testGroup "Encoding"
  [ testGroup "Workspace"
      [testDefaultWorkspace, testCustomWorkspace]
  , testGroup "Environment"
      [testDefaultEnvironment]
  , testGroup "Request"
      [testGetRequest, testPostRequest]
  ]

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

testGetRequest :: TestTree
testGetRequest = testCase "GET /abc/qwe" $
  toJSON (createRequest GET "/abc/qwe" defaultWorkspace) @?= [aesonQQ|
    {
      "_id": "Request_GET_/abc/qwe",
      "_type": "request",
      "url": "{{baseUrl}}/abc/qwe",
      "name": "/abc/qwe",
      "body": {},
      "method": "GET",
      "parentId": "Workspace_Servant-Insomnia"
    }|]

testPostRequest :: TestTree
testPostRequest = testCase "POST /abc/qwe" $
  toJSON (createRequest POST "/abc/qwe" defaultWorkspace) @?= [aesonQQ|
    {
      "_id": "Request_POST_/abc/qwe",
      "_type": "request",
      "url": "{{baseUrl}}/abc/qwe",
      "name": "/abc/qwe",
      "body": {},
      "method": "POST",
      "parentId": "Workspace_Servant-Insomnia"
    }|]
