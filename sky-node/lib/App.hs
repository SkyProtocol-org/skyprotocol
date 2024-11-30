{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App (runApp) where

import App.Env
import Config (AppConfig (..))
import Data.Functor (void)
import Effectful
import Effectful.Concurrent
import Effectful.Log
import Effectful.Reader.Static
import qualified Network.Socket as S
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (bracket, finally)

type AppEffects = '[Reader AppEnv, Log, Concurrent, IOE]

runApp :: Eff AppEffects a -> IO a
runApp = do
  env <- initAppEnv
  runEff $ do
    runLogStdout $ runConcurrent $ runReader env runServer

-- | Runs the server. Binds socket to the address and accepts incoming connection.
runServer :: AppM ()
runServer = do
  logMsg "Starting Sky Node..."
  config <- asks envConfig
  addr <- resolve config.hostname config.port
  bracket (liftIO $ S.openSocket addr) (liftIO . S.close) $ \sock -> do
    liftIO $ do
      S.setSocketOption sock S.ReuseAddr 1 -- easier for debugging
      S.bind sock (S.addrAddress addr)
      S.listen sock 10
    logMsg $ "Node listening on port " <> config.port
    acceptLoop sock
  where
    resolve :: String -> String -> AppM S.AddrInfo
    resolve host p = do
      let hints = S.defaultHints {S.addrSocketType = S.Stream}
      addr : _ <- liftIO $ S.getAddrInfo (Just hints) (Just $ host <> ":" <> p) Nothing
      pure addr

-- | Loop handling new connections.
acceptLoop :: S.Socket -> AppM ()
acceptLoop sock = do
  (conn, conn_addr) <- liftIO $ S.accept sock
  logMsg $ "Accepted new connection from " <> show conn_addr
  void $
    forkIO $
      handlePeer conn `finally` do
        logMsg $ "Closing connection to " <> show conn_addr
        liftIO $ S.close conn

handlePeer :: S.Socket -> AppM ()
handlePeer = undefined
