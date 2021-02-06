module API (tests, testApi) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Aeson
import Data.Aeson.QQ.Simple
import Servant
import Insomnia.Typelevel (HasInsomnia(..))

tests :: TestTree
tests = testGroup "Full"
  [testApiEncoding]

type TestApi = "books" :>
  (    Get '[JSON] [Int]
  :<|> (ReqBody '[JSON] Int :> Post '[] NoContent)
  :<|> Description "Book" :> Capture "id" Int :> Get '[JSON] Int
  )

testApi :: Proxy TestApi
testApi = Proxy

testApiEncoding :: TestTree
testApiEncoding = testCase "books" $
  toJSON (toInsomnia testApi "/") @?= [aesonQQ|
  {
    "_type": "export",
    "__export_format": 4,
    "__export_source": "servant-insomnia:v0.1",
    "resources": [
      {
        "_type": "workspace",
        "_id": "Workspace_Servant-Insomnia",
        "name": "Servant-Insomnia",
        "description": "",
        "parentId": null
      },
      {
        "_type": "environment",
        "_id": "Environment_of_Workspace_Servant-Insomnia",
        "data": {
          "baseUrl": "http://localhost:8080"
        }
      },
      {
        "_type": "request",
        "_id": "Request_GET_/books/",
        "body": {},
        "url": "{{baseUrl}}/books/",
        "name": "/books",
        "method": "GET",
        "parentId": "Workspace_Servant-Insomnia",
        "description": ""
      },
      {
        "_type": "request",
        "_id": "Request_POST_/books/",
        "body": {},
        "url": "{{baseUrl}}/books/",
        "name": "/books",
        "method": "POST",
        "parentId": "Workspace_Servant-Insomnia",
        "description": ""
      },
      {
        "_type": "request",
        "_id": "Request_GET_/books/:id/",
        "body": {},
        "url": "{{baseUrl}}/books/:id/",
        "name": "/books/:id",
        "method": "GET",
        "parentId": "Workspace_Servant-Insomnia",
        "description": "Book"
      }
    ]
  }
  |]
