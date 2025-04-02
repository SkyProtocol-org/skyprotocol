{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effect.PeerEff where

import App.Env (Topics, topics)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Data.Word (Word64)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Network.Socket qualified as S
import Types
import UnliftIO (catchIO, throwString)
import Utils

data PeerEff :: Effect where
  PublishBlock :: TopicId -> BlockData -> PeerEff m Bool
  GetTopics :: Maybe TopicId -> PeerEff m [TopicId]
  DescribeTopic :: TopicId -> PeerEff m TopicMetaData
  PollTopic :: TopicId -> PeerEff m (Word64, Bool)
  GetTopicBlockCertificate :: TopicId -> Word64 -> PeerEff m Bool
  ReadTopic :: TopicId -> Word64 -> PeerEff m [BlockData]

-- TODO replace with makeEffect_ and add docs for type sigs, or add docs to the GADT
makeEffect ''PeerEff

runPeerEffIO :: (Has Topics env, IOE :> es, Error String :> es, Reader env :> es) => S.Socket -> Eff (PeerEff : es) a -> Eff es a
runPeerEffIO _sock = interpret $ \_ -> \case
  PublishBlock tId bData ->
    undefined
  -- topics <- (.topics) <$> ask
  -- adapt $ atomically $ do
  -- tpcs <- readTVar topics
  -- let tpcs' = IntMap.adjust (\tpc -> tpc {messages = IntMap.insert (IntMap.size tpc.messages) bData tpc.messages}) tId.id tpcs
  -- writeTVar topics tpcs'
  GetTopics mtId -> adapt $ undefined
  DescribeTopic tId -> do
    undefined
  -- topics <- (.topics) <$> ask
  -- adapt $ do
  -- maybeMeta <- atomically $ do
  -- tpcs <- readTVar topics
  -- let mTpc = (.metadata) <$> (IntMap.!?) tpcs tId.id
  -- pure mTpc
  -- case maybeMeta of
  --   Just meta -> pure meta
  --   Nothing -> throwString "No such topic" -- TODO replace with proper error handling
  PollTopic tId -> adapt $ undefined
  GetTopicBlockCertificate tId height -> adapt $ undefined
  ReadTopic tId height -> adapt $ undefined
  where
    adapt m = liftIO m `catchIO` \e -> throwString $ show e
