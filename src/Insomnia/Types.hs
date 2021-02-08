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
  , description :: T.Text -- ^ generate it from Description and Summary
  , id :: T.Text -- ^ random id. May will be equal to url
  , headers :: [T.Text] -- ^ headers names list
  , params  :: [T.Text] -- ^ query parameters names list
  } deriving (Show, Eq, Generic)

-- | smart constructor for 'Request'
createRequest :: StdMethod -> T.Text -> Workspace -> Request
createRequest method url' Workspace{id=parentId} = Request{..}
  where
    url = "{{baseUrl}}" <> url'
    name = if T.last url' == '/'
      then T.init url' -- remove ending /
      else url'
    id = "Request_" <> (T.pack . show $ method) <> "_" <> url'
    description = ""
    params = []
    headers = []

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

-- | default 'Workspace' There is no way to use another workspace for now
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

data Insomnia = Insomnia
  { workspace :: Workspace
  , environment :: Environment
  , requests :: [Request]
  } deriving (Show, Eq, Generic)

instance Semigroup  Insomnia where
  x <> y = Insomnia w env rqs
    where
      w = workspace x
      env = environment x
      rqs = requests x <> requests y

instance Monoid Insomnia where
  mempty = Insomnia
    { workspace=defaultWorkspace
    , environment=createEnvironment defaultWorkspace
    , requests=[]
    }

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
    , "description" .= description
    , "body" .= object []
    , "headers" .= map crateHeaderObject headers
    , "parameters" .= map createParamObject params
    ]
    where
      crateHeaderObject name = object
        ["name" .= name, "value" .= ("" :: T.Text)]
      createParamObject name = object
        ["name" .= name, "value" .= ("" :: T.Text), "disabled" .= True]
instance ToJSON Workspace where
  toJSON Workspace{..} = object
    [ "_id" .= id
    , "_type" .= ("workspace" :: T.Text)
    , "name" .= name
    , "description" .= description
    , "parentId" .= Null
    ]

instance ToJSON Environment where
  toJSON Environment{..} = object
    [ "_id" .= id
    , "_type" .= ("environment" :: T.Text)
    , ("data", object . map (fmap String) $ pairs)
    ]

instance ToJSON Insomnia where
  toJSON Insomnia{..} = object
    [ "_type" .= ("export" :: T.Text)
    , "__export_format" .= (4 :: Int)
    , "__export_source" .= ("servant-insomnia:v0.1" :: T.Text)
    , "resources" .=
        ([toJSON workspace, toJSON environment] ++ map toJSON requests)
    ]
