{-# LANGUAGE DataKinds #-}
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
import Data.Trie qualified as Trie
import GHC.Records (HasField)
import Peer
import Types
import Utils

-- | Convenvince type alias to avoid writing full type in 'askField' invocations
type Topics = TVar (Trie.Trie TopicId Topic)

-- | Convenvince type alias to avoid writing full type in 'askField' invocations
type Peers = TVar [Peer]

data AppEnv = AppEnv
  { config :: AppConfig,
    topics :: Topics,
    peers :: Peers
  }

instance (HasField "config" AppEnv AppConfig) => Has AppConfig AppEnv where
  getField env = env.config

-- instance (HasField "topics" AppEnv Topics) => Has Topics AppEnv where
--   getField env = env.topics

instance (HasField "peers" AppEnv Peers) => Has Peers AppEnv where
  getField env = env.peers

-- | Initializes default node state.
initAppEnv :: AppConfig -> IO AppEnv
initAppEnv config = do
  peers <- newTVarIO []
  topics <- newTVarIO Trie.empty
  pure $ AppEnv {..}
