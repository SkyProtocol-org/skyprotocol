module OffChain.API (apiSpec) where

import API (api)
import Common.OffChain ()
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Test.Hspec

apiSpec :: Spec
apiSpec = describe "API Tests" $ do
  it "should return OK for health endpoint" $ do
    manager <- liftIO $ newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" 8080 ""
    let clientEnv = mkClientEnv manager baseUrl
    res <- liftIO $ runClientM healthClient clientEnv
    res `shouldBe` Right "OK"

-- Define Servant client functions
healthClient :<|> _ = client api
