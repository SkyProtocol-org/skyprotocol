{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OffChain.API (apiSpec) where

import API
import API.Da
import API.Types
import App hiding (getCardanoUser)
import Common
import Common.OffChain ()
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Log
import Log.Backend.LogList
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Servant.Client.Generic
import Test.Tasty
import Test.Tasty.HUnit
import Util

testEnv :: AppConfig -> Logger -> IO AppEnv
testEnv appConfig logger = do
  appAdmin <- getCardanoUser
  appOfferer <- getCardanoUser
  appClaimant <- getCardanoUser
  let (da, _schema, _committee) = createTestDa $ cuserVerificationKey appAdmin
      _blockState = initBlockState da
      appState = initAppState _blockState $ BridgeState da
  appStateW <- newMVar appState
  appStateR <- newMVar appState

  let appProviders = Nothing

  pure AppEnv {..}

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
  config <- loadYamlSettings ["config/test-config.yaml"] [] useEnv
  logger' <- mkLogger "tests" (putLogList appLogList)
  appEnv <- testEnv config logger'

  -- start the app
  appThreadId <- forkIO $ run (configPort config) $ app appEnv
  -- Give the server some time to start
  {- TODO: have the server print something or execute something we can recognize
     that won't be just confused with logging, to signal that it's ready,
     after it starts listening. Then we can wait for that message here instead of sleeping -}
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

-- TODO: add tests for the rest of the endpoints
skyClient :: SkyApi (AsClientT ClientM)
skyClient = genericClient

testUser :: BasicAuthData
testUser = BasicAuthData "skyAdmin" "1234"

-- NOTE: 'withResource' shares the resource across the 'testGroup' it is applied to
apiSpec :: TestTree
apiSpec = withResource startAPI closeAPI $ \getTestEnv ->
  testGroup
    "API Tests"
    [ testGroup
        "Health client"
        let healthC = skyClient // health
         in [ testCase "health endpoint" $ do
                TestEnv {..} <- getTestEnv
                res <- liftIO $ runClientM healthC clientEnv
                res @?= Right "OK"
            ],
      -- NOTE: this ensures, that the tests in this test group run in sequence
      sequentialTestGroup "Topic client" AllFinish $
        let protectedDaClient user = skyClient // da // protected /: user
            createTopicC = protectedDaClient testUser // createTopic
            publishMessageC = protectedDaClient testUser // publishMessage
            readMessageC = skyClient // da // public // readMessage
         in -- _getProof
            -- _readMessage = publicTopicApi
            [ testCase "should return TopicId 0 when creating new topic in empty da" $ do
                TestEnv {..} <- getTestEnv
                res <- liftIO $ runClientM createTopicC clientEnv
                res @?= Right (topicIdFromInteger 0),
              testCase "should return MessageId 0 when creating new message in an empty da with 1 topic" $ do
                TestEnv {..} <- getTestEnv
                res <- liftIO $ flip runClientM clientEnv $ publishMessageC (topicIdFromInteger 0) $ RawBytes "keklolarbidol"
                case res of
                  Right (mId, _) -> mId @?= messageIdFromInteger 0
                  Left _ -> assertFailure "Wrong message id",
              testCase "should return the same message we published in the previous test" $ do
                TestEnv {..} <- getTestEnv
                res <- liftIO $ flip runClientM clientEnv $ readMessageC (topicIdFromInteger 0) (messageIdFromInteger 0)
                res @?= Right (RawBytes "keklolarbidol")
            ]
    ]
