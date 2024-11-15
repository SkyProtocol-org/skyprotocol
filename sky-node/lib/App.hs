{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module App (runApp) where

-- import App.Error
import Config (AppConfig (..))
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.STM.TVar (TVar)
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)

-- | Aux data structure to keep info about 'Peer'.
newtype Peer = Peer
  { -- | Id of the 'Peer'
    id :: String
  }
  deriving (Show)

-- | Signalizes if the node needs to shutdown or continue running.
data Shutdown = Continue | Shutdown deriving (Show)

-- | State of the node.
data AppState = AppState
  { -- | Messages.
    messages :: TVar [Int],
    -- | List of peers that are connected to this node.
    peers :: [Peer],
    -- | Marker for graceful shutdown.
    continue :: Shutdown
  }

-- | Initializes default node state
initAppState :: IO AppState
initAppState = do
  let peers = []
      continue = Continue
  messages <- newTVarIO []
  pure $ AppState {..}

-- | Run the application
runApp :: AppConfig -> LoggerSet -> IO ()
runApp config logger = do
  state <- initAppState
  pushLogStrLn logger . toLogStr $ "Starting Sky Node on port " <> config.port
  runServer config state logger

runServer :: AppConfig -> AppState -> LoggerSet -> IO ()
runServer config state logger = undefined
