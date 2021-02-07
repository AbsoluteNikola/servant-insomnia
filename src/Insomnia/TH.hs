module Insomnia.TH where

import Language.Haskell.TH
import Insomnia.Typelevel
import Insomnia.Types
import Data.Proxy
import Data.Typeable
import Data.Aeson


{- |
Generate insomnia config from

Example
@
type Api = "hello" :> Get '[JSON] String

writeApiToInsomniaFile "./insomnia.json" (Proxy :: Proxy Api)
@
-}
writeApiToInsomniaFile
  :: (Typeable api, HasInsomnia api)
  => FilePath -- ^ file path where config will be written
  -> Proxy api -- ^ servant api
  -> Q [Dec] -- ^ will be empty list, doesn't affect existing code
writeApiToInsomniaFile path p = do
  runIO $ do
    encodeFile path . toInsomnia p $ "/"
  pure []
