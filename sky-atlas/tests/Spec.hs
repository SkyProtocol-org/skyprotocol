module Main (main) where

import Test.Hspec
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import API (api)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = hspec $ do
  describe "API Tests" $ do
    it "should return OK for health endpoint" $ do
      manager <- liftIO $ newManager defaultManagerSettings
      let baseUrl = BaseUrl Http "localhost" 8080 ""
      let clientEnv = mkClientEnv manager baseUrl
      res <- liftIO $ runClientM healthClient clientEnv
      res `shouldBe` Right "OK"

-- Define Servant client functions
healthClient :<|> _ = client api
