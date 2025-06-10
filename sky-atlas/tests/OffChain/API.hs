module OffChain.API (apiSpec) where

import API
import Common
import Common.OffChain ()
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Text
import Log
import Log.Backend.LogList
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Test.Tasty

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
  logger' <- mkLogger "tests" (putLogList appLogList)
  appEnv <- testEnv logger'

  -- start the app
  appThreadId <- forkIO $ startApp appEnv
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

-- Helper function to manage the lifecycle of the API server
withAPI :: (TestEnv -> IO ()) -> IO ()
withAPI = bracket startAPI closeAPI

-- Define Servant client functions
healthClient :: ClientM Text
_bridgeClient :: ClientM Text :<|> (Text -> ClientM Text)
createTopic :: ClientM TopicId
readTopic :: TopicId -> MessageId -> ClientM BS.ByteString
updateTopic :: Text -> ClientM Text
healthClient :<|> _bridgeClient :<|> (createTopic :<|> readTopic :<|> updateTopic) = client api

apiSpec :: TestTree
apiSpec = testGroup "API Tests"
  [ testCase "should return OK for health endpoint" $ withAPI $ \TestEnv {..} -> do
      res <- liftIO $ runClientM healthClient clientEnv
      res @?= Right "OK"

  , testCase "should return TopicId 0 when creating new topic" $ withAPI $ \TestEnv {..} -> do
      res <- liftIO $ runClientM createTopic clientEnv
      res @?= Right (topicIdFromInteger 0)
  ]
