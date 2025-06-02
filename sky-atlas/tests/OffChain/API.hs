module OffChain.API (apiSpec) where

import Common.OffChain ()
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Test.Hspec
import API
import Control.Exception (bracket)
import Log.Backend.LogList
import Log
import Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)

-- Start the API server in a separate thread
startAPI :: IO (AppEnv, ThreadId, LogList)
startAPI = do
  logList <- newLogList
  logger <- mkLogger "tests" (putLogList logList)
  env <- testEnv logger
  appThreadId <- forkIO $ startApp env
  -- Give the server some time to start
  threadDelay 1000000
  pure (env, appThreadId, logList)

-- Shut down the API server and display logs
closeAPI :: (AppEnv, ThreadId, LogList) -> IO ()
closeAPI (AppEnv {..}, appThreadId, logList) = do
  -- Kill the server thread
  killThread appThreadId
  -- Wait for logger to flush logs
  waitForLogger logger
  -- Display logs
  putStrLn "Shutting down server. Logs:"
  mapM_ print =<< getLogList logList
  -- Shutdown the logger
  shutdownLogger logger

-- Helper function to manage the lifecycle of the API server
withAPI :: ((AppEnv, ThreadId, LogList) -> IO ()) -> IO ()
withAPI = bracket startAPI closeAPI

-- Define Servant client functions
healthClient :<|> _ = client api

apiSpec :: Spec
apiSpec = around withAPI $ describe "API Tests" $ do
  it "should return OK for health endpoint" $ \(AppEnv {..}, _appThreadId, _logList) -> do
    manager <- liftIO $ newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" (configPort appConfig) ""
    let clientEnv = mkClientEnv manager baseUrl
    res <- liftIO $ runClientM healthClient clientEnv
    res `shouldBe` Right "OK"
