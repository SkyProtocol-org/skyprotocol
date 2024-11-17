{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module App (runApp) where

-- import App.Error
import Config (AppConfig (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (bracket)
import Control.Exception.Base (finally)
import Control.Monad (when)
import Data.Default
import qualified Network.Socket as S
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)

-- | Aux data structure to keep info about 'Peer'.
newtype Peer = Peer
  { -- | Id of the 'Peer'
    id :: String
  }
  deriving (Show)

-- | Signalizes if the node needs to shutdown or continue running.
data Shutdown = Continue | Shutdown deriving (Show, Eq, Bounded)

instance Default Shutdown where
  def = Continue

-- | State of the node.
data AppState = AppState
  { -- | Messages.
    messages :: TVar [Int],
    -- | List of peers that are connected to this node.
    peers :: [Peer],
    -- | Marker for graceful shutdown.
    continue :: Shutdown
  }

-- | Initializes default node state.
initAppState :: IO AppState
initAppState = do
  let peers = def
      continue = def
  messages <- newTVarIO def
  pure $ AppState {..}

-- | Runs the application with the default 'AppState'.
runApp :: AppConfig -> LoggerSet -> IO ()
runApp config logger = do
  state <- initAppState
  pushLogStrLn logger $ toLogStr "Starting Sky Node..."
  runServer config state logger

-- | Runs the server. Binds socket to the address and accepts incoming connection.
runServer :: AppConfig -> AppState -> LoggerSet -> IO ()
runServer config state logger = do
  addr <- resolve config.hostname config.port
  bracket (S.openSocket addr) S.close $ \sock -> do
    S.setSocketOption sock S.ReuseAddr 1 -- easier for debugging
    S.bind sock (S.addrAddress addr)
    S.listen sock 10
    pushLogStrLn logger . toLogStr $ "Node listening on port " <> config.port
    acceptLoop sock state logger
  where
    resolve :: String -> String -> IO S.AddrInfo
    resolve host port = do
      let hints = S.defaultHints {S.addrSocketType = S.Stream}
      addr : _ <- S.getAddrInfo (Just hints) (Just $ host <> ":" <> port) Nothing
      pure addr

-- | Loop handling new connections.
acceptLoop :: S.Socket -> AppState -> LoggerSet -> IO ()
acceptLoop sock AppState {..} logger = do
  (conn, conn_addr) <- S.accept sock
  pushLogStrLn logger . toLogStr $ "Accepted new connection from " <> show conn_addr
  _ <-
    forkIO $
      handlePeer conn AppState {..} logger `finally` do
        S.close conn

  -- loop until told to shutdown
  when (continue == Continue) $ acceptLoop sock AppState {..} logger

handlePeer :: S.Socket -> AppState -> LoggerSet -> IO ()
handlePeer = undefined
