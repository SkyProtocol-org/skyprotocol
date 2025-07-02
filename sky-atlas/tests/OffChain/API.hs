{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OffChain.API (apiSpec) where

import API
import App
import Common
import Common.OffChain ()
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GeniusYield.GYConfig (withCfgProviders)
import GeniusYield.Types
import Log
import Log.Backend.LogList
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Test.Tasty
import Test.Tasty.HUnit

testSecKey1 :: SecKey
testSecKey1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"

testSecKey2 :: SecKey
testSecKey2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"

testPubKey1 :: PubKey
testPubKey1 = derivePubKey testSecKey1

testPubKey2 :: PubKey
testPubKey2 = derivePubKey testSecKey2

testEnv :: AppConfig -> Logger -> GYProviders -> IO AppEnv
testEnv appConfig logger appProviders = do
  let daSchema = computeHash (ofHex "deadbeef" :: Bytes4)
      committee = MultiSigPubKey ([testPubKey1, testPubKey2], UInt16 2)
      _skyDa = runIdentity $ initDa daSchema committee :: SkyDa HashRef
      _blockState =
        BlockState
          { _skyDa,
            _topic = (),
            _erasureCoding = (),
            _superTopic = (),
            _subTopics = (),
            _publisherPayments = ()
          }
      appState =
        AppState
          { _blockState,
            _oldBlockQueue = (),
            _partialSignatures = (),
            _bridgeState = BridgeState _skyDa,
            _stake = (),
            _peers = (),
            _clients = (),
            _subscriberPayments = (),
            _auctions = (),
            _longTermStorage = ()
          }
  appStateW <- newMVar appState
  appStateR <- newMVar appState

  appAdmin <- getCardanoUser "config/admin/"
  appOfferer <- getCardanoUser "config/offerer/"
  appClaimant <- getCardanoUser "config/claimant/"

  pure $ AppEnv {..}

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

-- TODO: add tests for the rest of the endpoints
healthClient
  :<|> _bridgeClient
  :<|> ( (_readTopic :<|> _getProof :<|> _readMessage)
           :<|> protectedTopicApi
         ) = client api

testUser :: BasicAuthData
testUser = BasicAuthData "skyAdmin" "1234"

createTopic :<|> _publishTopicMessage = protectedTopicApi testUser

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
        res <- liftIO $ runClientM createTopic clientEnv
        res @?= Right (topicIdFromInteger 0)
    ]
