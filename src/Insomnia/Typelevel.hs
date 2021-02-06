module Insomnia.Typelevel where

import Insomnia.Types
import Network.HTTP.Types (StdMethod(..))
import Data.Data
import qualified Data.Text as T

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
