{-# OPTIONS_GHC -Wno-missing-signatures #-}
module OffChain.API (apiSpec) where

import API
import Common
import Common.OffChain ()
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GeniusYield.GYConfig (withCfgProviders)
import Log
import Log.Backend.LogList
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Test.Tasty
import Test.Tasty.HUnit
import API.Types

data TestEnv = TestEnv
  { appEnv :: AppEnv,
    appThreadId :: ThreadId,
    appLogList :: LogList,
    clientEnv :: ClientEnv
  }

-- Start the API server in a separate thread
startAPI :: IO TestEnv
startAPI = do
  -- initialize App environment
  appLogList <- newLogList
  config <- loadYamlSettings ["config/local-test.yaml"] [] useEnv
  logger' <- mkLogger "tests" (putLogList appLogList)
  withCfgProviders (configAtlas config) "api-test" $ \providers -> do
    appEnv <- testEnv config logger' providers

    -- start the app
    appThreadId <- forkIO $ run (configPort config) $ app appEnv
    -- Give the server some time to start
    threadDelay 1000000

    -- initialize http client
    manager <- liftIO $ newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" (configPort $ appConfig appEnv) ""
        clientEnv = mkClientEnv manager baseUrl
    pure TestEnv {..}

-- Shut down the API server and display logs
closeAPI :: TestEnv -> IO ()
closeAPI TestEnv {..} = do
  -- Kill the server thread
  killThread appThreadId
  -- Wait for logger to flush logs
  waitForLogger $ logger appEnv
  -- Display logs
  putStrLn "Shutting down server. Logs:"
  mapM_ print =<< getLogList appLogList
  -- Shutdown the logger
  shutdownLogger $ logger appEnv

healthClient :<|> _bridgeClient :<|> ((_readTopic :<|> _getProof) :<|> createTopic :<|> publishMessage) = client api

testUser :: BasicAuthData
testUser = BasicAuthData "skyAdmin" "1234"

-- NOTE: 'withResource' shares the resource across the 'testGroup' it is applied to
apiSpec :: TestTree
apiSpec = withResource startAPI closeAPI $ \getTestEnv ->
  testGroup
    "API Tests"
    [ testCase "should return OK for health endpoint" $ do
        TestEnv {..} <- getTestEnv
        res <- liftIO $ runClientM healthClient clientEnv
        res @?= Right "OK",
      testCase "should return TopicId 0 when creating new topic" $ do
        TestEnv {..} <- getTestEnv
        res <- liftIO $ runClientM (createTopic testUser) clientEnv
        res @?= Right (topicIdFromInteger 0)
    ]
