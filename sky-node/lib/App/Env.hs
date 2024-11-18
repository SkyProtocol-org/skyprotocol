{-# LANGUAGE RecordWildCards #-}

module App.Env
  ( AppEnv (..),
    initAppEnv,
  )
where

import Config
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Default
import Peer
import System.Log.FastLogger (LoggerSet)

data AppEnv = AppEnv
  { envConfig :: AppConfig,
    envMessages :: TVar [Int],
    envPeers :: TVar [Peer],
    envLogger :: LoggerSet
  }

-- | Initializes default node state.
initAppEnv :: AppConfig -> LoggerSet -> IO AppEnv
initAppEnv envConfig envLogger = do
  envPeers <- newTVarIO def
  envMessages <- newTVarIO def
  pure $ AppEnv {..}
