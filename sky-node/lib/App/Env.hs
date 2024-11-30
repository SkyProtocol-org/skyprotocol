{-# LANGUAGE RecordWildCards #-}

module App.Env
  ( AppEnv (..),
    initAppEnv,
  )
where

import Config
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Default
import Data.IntMap.Strict (IntMap)
import Peer
import Types

data AppEnv = AppEnv
  { envConfig :: AppConfig,
    envMessages :: TVar (IntMap Topic),
    envPeers :: TVar [Peer]
  }

-- | Initializes default node state.
initAppEnv :: AppConfig -> IO AppEnv
initAppEnv envConfig = do
  envPeers <- newTVarIO def
  envMessages <- newTVarIO def
  pure $ AppEnv {..}
