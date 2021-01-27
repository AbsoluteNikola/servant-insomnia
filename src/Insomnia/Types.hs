module Insomnia.Types where

import Network.HTTP.Types (StdMethod)
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T
import Prelude hiding (id)

data Request = Request
  { method :: StdMethod -- ^ type of request
  , parentId :: T.Text -- ^ should be equal to id of Workspace
  , url :: T.Text
    -- ^ path (e.g /book/1) under the hood {{baseUrl}} will be added
  , name :: T.Text -- ^ will be equal to url, but may be overwritten
  , id :: T.Text -- ^ random id. May will be equal to url
  } deriving (Show, Eq, Generic)

-- | smart constructor for 'Request'
createRequest :: StdMethod -> Workspace -> T.Text -> Request
createRequest method Workspace{id=parentId} url' = Request{..}
  where
    url = "{{baseUrl}}" <> url'
    name = url'
    id = "Request_" <> (T.pack . show $ method) <> "_" <> url'

data Workspace = Workspace
  { name :: T.Text -- ^ will be servant-insomnia if was not overwritten
  , description :: T.Text -- ^ I don't sure that it useful
  , id :: T.Text -- ^ All other types will have parentId equal to id
  } deriving (Show, Eq, Generic)

-- | smart constructor for 'Workspace'
createWorkspace :: T.Text -> T.Text -> Workspace
createWorkspace name description = Workspace{..}
  where
    id = "Workspace_" <> name

-- | default 'Workspace'
defaultWorkspace :: Workspace
defaultWorkspace = createWorkspace "Servant-Insomnia" ""

data Environment = Environment
  { pairs :: [(T.Text, T.Text)]
    -- ^ list kev-values where key is a name of variable (baseName for example)
    -- and value of this variable
  , parentId :: T.Text -- ^ same as in other types
  , id :: T.Text -- ^ random value
  } deriving (Show, Eq, Generic)

-- | smart constructor for 'Environment'
createEnvironment :: Workspace -> Environment
createEnvironment Workspace{id=parentId} = Environment{..}
  where
    pairs = [("baseUrl", "http://localhost:8080")]
    id = "Environment_of_" <> parentId

instance ToJSON StdMethod where
  toJSON  = String . T.pack . show

instance ToJSON Request where
  toJSON Request{..} = object
    [ "_id" .= id
    , "_type" .= ("request" :: T.Text)
    , "method" .= method
    , "name" .= name
    , "url" .= url
    , "parentId" .= parentId
    ]

instance ToJSON Workspace where
  toJSON Workspace{..} = object
    [ "_id" .= id
    , "_type" .= ("workspace" :: T.Text)
    , "name" .= name
    , "parentId" .= Null
    ]

instance ToJSON Environment where
  toJSON Environment{..} = object
    [ "_id" .= id
    , "_type" .= ("environment" :: T.Text )
    , ("data", object . map (fmap String) $ pairs)
    ]
