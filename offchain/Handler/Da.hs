module Handler.Da where

import App
import Common as C
import Contract.DaH
import Control.Concurrent.MVar
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Data.ByteString qualified as BS
import Log.Class
import PlutusLedgerApi.Common (fromBuiltin, toBuiltin)
import Utils (currentPOSIXTime)

readTopicHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m
  ) =>
  TopicId ->
  MessageId ->
  m BS.ByteString
readTopicHandler tId mId = do
  stateR <- asks appStateR
  state <- liftIO $ readMVar stateR
  let SkyDa {..} = state._blockState._skyDa
  maybeTopic <- C.lookup tId =<< unwrap skyTopicTrie
  case maybeTopic of
    Nothing -> throwError . APIError $ "Can't find topic with id: " <> show (toInt tId)
    Just (_tMeta, messageTrie) -> do
      maybeMessage <- C.lookup mId =<< unwrap messageTrie
      case maybeMessage of
        Nothing -> throwError . APIError $ "Can't find message with id: " <> show (toInt mId)
        Just (_mMeta, mData) -> fromBuiltin <$> unwrap mData

createTopicHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadLog m
  ) =>
  m TopicId
createTopicHandler = do
  stateW <- asks appStateW
  stateR <- asks appStateR
  topicId <- liftIO . modifyMVar stateW $ \state -> do
    let da = state._blockState._skyDa
    let (newDa, maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da
    case maybeTopicId of
      Nothing -> throwError $ userError "Can't add topic"
      Just topicId -> do
        let newBlockState = state._blockState {_skyDa = newDa}
            newState = state {_blockState = newBlockState}
        modifyMVar_ stateR . const . pure $ newState
        pure (newState, topicId)
  logTrace "Created topic " topicId
  pure topicId

publishMessageHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m,
    MonadLog m
  ) =>
  TopicId ->
  BS.ByteString ->
  m (MessageId, Hash)
publishMessageHandler tId msgBody = do
  stateW <- asks appStateW
  stateR <- asks appStateR
  maybeMessageId <- liftIO . modifyMVar stateW $ \state -> do
    let da = state._blockState._skyDa
    timestamp <- currentPOSIXTime
    let (newDa, maybeMessageId) = runIdentity $ C.insertMessage timestamp (toBuiltin msgBody) tId da
    let newBlockState = state._blockState {_skyDa = newDa}
        newState = (state {_blockState = newBlockState}, maybeMessageId)
    -- (set (blockState . skyDa) newDa state, maybeMessageId)
    modifyMVar_ stateR . const . pure $ fst newState
    pure newState
  case maybeMessageId of
    Nothing -> throwError $ APIError "publishMessage failed"
    Just messageId -> do
      logTrace "Published message" messageId
      return (messageId, computeDigest @Hash $ toBuiltin msgBody)

getProofHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m
  ) =>
  TopicId ->
  MessageId ->
  m BS.ByteString
getProofHandler tId mId = do
  stateR <- asks appStateR
  state <- liftIO . readMVar $ stateR
  let da = state._bridgeState._bridgedSkyDa
      maybeRmdProof = runIdentity $ getSkyDataProofH (tId, mId) da :: Maybe (Hash, SkyDataProofH)
  case maybeRmdProof of
    Nothing -> throwError $ APIError "Message not found. Maybe wait for bridge update?"
    Just (_rmd, proof) ->
      pure . fromBuiltin . toByteString $ proof
