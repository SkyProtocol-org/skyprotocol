{-# LANGUAGE DataKinds #-}
-- while looks scary, it's needed because GHC can't guarantee HasField instances. Just be careful when defining them.
{-# LANGUAGE UndecidableInstances #-}

module App.Env
  ( AppEnv (..),
    initAppEnv,
    Topics,
    Peers,
  )
where

import Config
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Default
import Data.IntMap.Strict (IntMap)
import GHC.Records (HasField)
import Peer
import Types
import Utils

-- | Convenvince type alias to avoid writing full type in 'askField' invocations
type Topics = TVar (IntMap Topic)

-- | Convenvince type alias to avoid writing full type in 'askField' invocations
type Peers = TVar [Peer]

data AppEnv = AppEnv
  { config :: AppConfig,
    topics :: Topics,
    peers :: Peers
  }

instance (HasField "config" AppEnv AppConfig) => Has AppConfig AppEnv where
  getField env = env.config

instance (HasField "topics" AppEnv Topics) => Has Topics AppEnv where
  getField env = env.topics

instance (HasField "peers" AppEnv Peers) => Has Peers AppEnv where
  getField env = env.peers

-- | Initializes default node state.
initAppEnv :: AppConfig -> IO AppEnv
initAppEnv config = do
  peers <- newTVarIO def
  topics <- newTVarIO def
  pure $ AppEnv {..}
