{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Peer.Handlers where

import App.Env
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.TH (makeEffect)
import Types
import UnliftIO (catchIO, throwString)

data PeerHandler :: Effect where
  PublishBlock :: TopicId -> BlockData -> PeerHandler m (Certificate Block)
  GetTopics :: Maybe TopicId -> PeerHandler m [TopicId]
  DescribeTopic :: TopicId -> PeerHandler m TopicMetaData
  PollTopic :: TopicId -> PeerHandler m (Word64, Certificate Block)
  GetTopicBlockCertificate :: TopicId -> Word64 -> PeerHandler m (Certificate Block)
  ReadTopic :: TopicId -> Word64 -> PeerHandler m [BlockData]

-- TODO replace with makeEffect_ and add docs for type sigs, or add docs to the GADT
makeEffect ''PeerHandler

runPeerHandlerIO :: (IOE :> es, Error String :> es, Reader AppEnv :> es) => Eff (PeerHandler : es) a -> Eff es a
runPeerHandlerIO = interpret $ \_ -> \case
  PublishBlock tId bData -> do
    topics <- asks envMessages
    adapt $ atomically $ do
      tpcs <- readTVar topics
      let tpcs' = IntMap.adjust (\tpc -> tpc {topicMessages = IntMap.insert (IntMap.size (topicMessages tpc)) bData (topicMessages tpc)}) (undefined {- this must be TopicId -}) tpcs
      writeTVar topics tpcs'
    pure $ makeBlockCertificate bData
  GetTopics mtId -> adapt $ undefined
  DescribeTopic tId -> do
    topics <- asks envMessages
    adapt $ do
      maybeMeta <- atomically $ do
        tpcs <- readTVar topics
        let mTpc = topicMeta <$> (IntMap.!?) tpcs (undefined {- this must be TopicId -})
        pure mTpc
      case maybeMeta of
        Just meta -> pure meta
        Nothing -> throwString "No such topic"
  PollTopic tId -> adapt $ undefined
  GetTopicBlockCertificate tId height -> adapt $ undefined
  ReadTopic tId height -> adapt $ undefined
  where
    adapt m = liftIO m `catchIO` \e -> throwString $ show e
