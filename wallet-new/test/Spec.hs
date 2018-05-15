module Main where

import           Universum

import qualified APISpec as API
import qualified DevelopmentSpec as Dev
import qualified MarshallingSpec as Marshalling
import qualified RequestSpec as ReqSpec
import qualified SwaggerSpec as Swagger
import           Test.Hspec
import qualified WalletHandlersSpec as WalletHandlers

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
main :: IO ()
main = hspec $ do
    Dev.spec
    Marshalling.spec
    API.spec
    Swagger.spec
    ReqSpec.spec
    WalletHandlers.spec
