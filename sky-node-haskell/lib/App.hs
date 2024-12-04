{-# LANGUAGE DataKinds #-}

module App (initApp) where

import App.Env
import Config (AppConfig (..))
import Control.Monad (forever)
import Data.Text (pack, unpack)
import Effect.PeerEff
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Error.Static (Error, runError)
import Effectful.Log
import Effectful.Reader.Static
import qualified Network.Socket as S
import UnliftIO.Exception (bracket)
import Utils

type AppEffects = '[Reader AppEnv, Concurrent, Error String, Log, IOE]

initApp :: AppConfig -> Logger -> IO ()
initApp config logger = do
  env <- initAppEnv config
  runEff $ do
    runLog "main" logger defaultLogLevel $ do
      -- we don't want handlers to interrupt the node with it's exceptions
      eRes <- runError $
        runConcurrent $
          runReader env $ do
            withSocketServer handlePeer
      case eRes of
        Left err -> logAttention_ $ pack $ show err
        Right res -> logInfo_ $ pack $ show res

withSocketServer :: (S.Socket -> Eff AppEffects ()) -> Eff AppEffects ()
withSocketServer handler = do
  logInfo_ "Starting Sky Node..."
  config <- askFieldS @AppConfig
  bracket setupServerSocket (liftIO . S.close) $ \sock -> do
    logInfo_ $ "Node listening on " <> config.host <> ":" <> config.port
    forever $ do
      (conn, addr) <- liftIO $ S.accept sock
      logInfo_ $ "Connection accepted from " <> pack (show addr)
      -- using withAsync ensures proper thread handling in the face of execption
      withAsync (handler conn) $ \_ -> pure ()

setupServerSocket :: Eff AppEffects S.Socket
setupServerSocket = do
  h <- (.host) <$> askFieldS @AppConfig
  p <- (.port) <$> askFieldS @AppConfig
  addrs <- liftIO $ S.getAddrInfo (Just S.defaultHints {S.addrSocketType = S.Stream}) (Just $ unpack h) (Just $ unpack p)
  let addr = head addrs
  sock <- liftIO $ S.openSocket addr
  liftIO $ do
    S.setSocketOption sock S.ReuseAddr 1 -- easier for debugging
    S.bind sock (S.addrAddress addr)
    S.listen sock 10
  pure sock

handlePeer :: S.Socket -> Eff AppEffects ()
handlePeer sock = runPeerEffIO sock $ do
  undefined
