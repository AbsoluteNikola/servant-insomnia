{-# LANGUAGE TypeApplications #-}
module Insomnia.Typelevel where

import Insomnia.Types
import Network.HTTP.Types (StdMethod(..))
import Servant
import Data.Data
import qualified Data.Text as T
import GHC.TypeLits

class FromStdMethodType (method :: StdMethod) where
  fromStdMethodType :: Proxy method -> StdMethod

instance FromStdMethodType 'GET     where fromStdMethodType _ = GET
instance FromStdMethodType 'POST    where fromStdMethodType _ = POST
instance FromStdMethodType 'PUT     where fromStdMethodType _ = PUT
instance FromStdMethodType 'DELETE  where fromStdMethodType _ = DELETE
instance FromStdMethodType 'OPTIONS where fromStdMethodType _ = OPTIONS
instance FromStdMethodType 'HEAD    where fromStdMethodType _ = HEAD
instance FromStdMethodType 'PATCH   where fromStdMethodType _ = PATCH

class HasInsomnia api where
  toInsomnia
    :: Proxy api -- ^ Servant api type
    -> T.Text -- ^ currentPath
    -> Insomnia -- ^ collected requests with workspace and environment

instance HasInsomnia EmptyAPI where
  toInsomnia _ = mempty

instance HasInsomnia api => HasInsomnia (api :> Raw) where
  toInsomnia _ _ = mempty

instance (HasInsomnia a, HasInsomnia b) => HasInsomnia (a :<|> b) where
  toInsomnia _ currentPath = a <> b
    where
      a = toInsomnia (Proxy :: Proxy a) currentPath
      b = toInsomnia (Proxy :: Proxy b) currentPath

instance (KnownSymbol path, HasInsomnia api) => HasInsomnia (path :> api) where
  toInsomnia _ currentPath =
    toInsomnia (Proxy :: Proxy api) (currentPath <> pathPart <> "/")
    where
      pathPart = T.pack $ symbolVal (Proxy :: Proxy path)

-- TODO: add query params support
instance HasInsomnia api => HasInsomnia (QueryParam' mods name t :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

-- TODO: add query flags support
instance HasInsomnia api => HasInsomnia (QueryFlag name :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

-- TODO: add with the same functionality as simple param
instance HasInsomnia api => HasInsomnia (QueryParams name t :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

-- TODO: generate headers from this combinator
instance HasInsomnia api => HasInsomnia (Header' mods name t :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

-- TODO: generate body type from this combinator
instance HasInsomnia api
  => HasInsomnia (ReqBody' mods contentTypes t :> api) where
    toInsomnia _ = toInsomnia (Proxy :: Proxy api)

instance FromStdMethodType method
  => HasInsomnia (Verb (method::StdMethod) statusCode contentTypes t) where
    toInsomnia _ currentPath = mempty {requests=[request]}
      where
        request = createRequest method currentPath defaultWorkspace
        method = fromStdMethodType (Proxy :: Proxy method)

-- | This combinator is used for custom auth. I don't know what to do with it
-- therefore just skip
instance HasInsomnia api => HasInsomnia (AuthProtect tag :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

-- I don't know what to do with it therefore just skip
instance HasInsomnia api => HasInsomnia (CaptureAll description t :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

-- | If you have capture you will need to modify you paths inside the app
instance (KnownSymbol name, HasInsomnia api)
  => HasInsomnia (Capture' mods name t :> api) where
    toInsomnia _ currentPath =
      toInsomnia (Proxy :: Proxy api) (currentPath <> paramName)
      where
        paramName = ":" <> (T.pack . symbolVal $ (Proxy :: Proxy name)) <> "/"

updateRequestsWithDescription
  :: KnownSymbol desc
  => Insomnia
  -> Proxy desc
  -> Insomnia
updateRequestsWithDescription ins p = ins{requests=updatedRequests}
  where
    desc = T.pack $ symbolVal p
    updatedRequests = map addDescToRequest $ requests ins
    -- Add new description only if request doesn't have it
    -- due to this descriptions near endpoints have priority
    addDescToRequest :: Request -> Request
    addDescToRequest req = if T.null (description (req::Request))
      then req{description=desc}
      else req

instance (KnownSymbol desc, HasInsomnia api)
  => HasInsomnia (Description desc :> api) where
    toInsomnia _ currentPath =
      updateRequestsWithDescription ins (Proxy :: Proxy desc)
      where
        ins = toInsomnia (Proxy :: Proxy api) currentPath

instance (KnownSymbol desc, HasInsomnia api)
  => HasInsomnia (Summary desc :> api) where
    toInsomnia _ currentPath =
      updateRequestsWithDescription ins (Proxy :: Proxy desc)
      where
        ins = toInsomnia (Proxy :: Proxy api) currentPath


-- These instances don't change anything in config

instance HasInsomnia api => HasInsomnia (HttpVersion :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

instance HasInsomnia api => HasInsomnia (Vault :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

instance HasInsomnia api => HasInsomnia (IsSecure :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

instance HasInsomnia api => HasInsomnia (WithNamedContext name c api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)

instance HasInsomnia api => HasInsomnia (RemoteHost :> api) where
  toInsomnia _ = toInsomnia (Proxy :: Proxy api)
