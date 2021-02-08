module TH (tests) where

import Servant
import Insomnia.TH
import Insomnia.Typelevel
import Insomnia.Types
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Aeson
import Data.Aeson.QQ.Simple

type Api = "hello" :> Get '[JSON] String

writeApiToInsomniaFile "./test/insomnia.json" (Proxy :: Proxy Api)

tests :: TestTree
tests = testGroup "Template Haskell"
  [ testCase "writeApiToInsomniaFile" $ do
      ins <- readInsomniaFile
      ins @?= [aesonQQ|
      {
        "__export_format": 4,
        "__export_source": "servant-insomnia:v0.1",
        "_type": "export",
        "resources": [
          {
            "_id": "Workspace_Servant-Insomnia",
            "_type": "workspace",
            "description": "",
            "name": "Servant-Insomnia",
            "parentId": null
          },
          {
            "_id": "Environment_of_Workspace_Servant-Insomnia",
            "_type": "environment",
            "parentId": "Workspace_Servant-Insomnia",
            "data": {
              "baseUrl": "http://localhost:8080"
            }
          },
          {
            "_id": "Request_GET_/hello/",
            "_type": "request",
            "body": {},
            "description": "",
            "method": "GET",
            "name": "/hello",
            "parentId": "Workspace_Servant-Insomnia",
            "url": "{{baseUrl}}/hello/",
            "headers": [],
            "parameters": []
          }
        ]
      }
      |]
  ]

readInsomniaFile :: IO Value
readInsomniaFile = do
  jsonData <- decodeFileStrict "./test/insomnia.json"
  case jsonData of
    Nothing -> error "Invalid json data"
    Just x -> pure x
