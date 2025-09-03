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
import Data.Text (pack)
import Log.Class
import PlutusLedgerApi.Common (fromBuiltin, toBuiltin)
import Utils (currentPOSIXTime)

readTopicHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m,
    MonadLog m
  ) =>
  TopicId ->
  MessageId ->
  m BS.ByteString
readTopicHandler tId mId = do
  stateR <- asks appStateR
  state <- liftIO $ readMVar stateR
  let SkyDa {..} = state.blockState.skyDa
  maybeTopic <- C.lookup tId =<< unwrap skyTopicTrie
  case maybeTopic of
    Nothing -> do
      logAttention_ $ "Can't find topic with id: " <> pack (show $ toInt tId)
      throwError . DaError $ "Can't find topic with id: " <> show (toInt tId)
    Just (_tMeta, messageTrie) -> do
      maybeMessage <- C.lookup mId =<< unwrap messageTrie
      case maybeMessage of
        Nothing -> do
          logAttention_ $ "Can't find message with id: " <> pack (show $ toInt mId)
          throwError . DaError $ "Can't find message with id: " <> show (toInt mId)
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
  tId <- liftIO . modifyMVar stateW $ \state -> do
    let da = state.blockState.skyDa
    let (newDa, maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da
    case maybeTopicId of
      Nothing -> throwError $ userError "Can't add topic"
      Just tId -> do
        let newBlockState = state.blockState {skyDa = newDa}
            newState = state {blockState = newBlockState}
        modifyMVar_ stateR . const . pure $ newState
        pure (newState, tId)
  logTrace_ $ "Created topic with id: " <> pack (show $ toInt tId)
  pure tId

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
    let da = state.blockState.skyDa
    timestamp <- currentPOSIXTime
    let (newDa, maybeMessageId) = runIdentity $ C.insertMessage timestamp (toBuiltin msgBody) tId da
    let newBlockState = state.blockState {skyDa = newDa}
        newState = (state {blockState = newBlockState}, maybeMessageId)
    modifyMVar_ stateR . const . pure $ fst newState
    pure newState
  case maybeMessageId of
    Nothing -> do
      logAttention_ "Failed to publish message"
      throwError $ DaError "Failed to publish message"
    Just mId -> do
      logTrace_ $ "Published message" <> pack (show $ toInt mId)
      pure (mId, computeDigest @Hash $ toBuiltin msgBody)

getProofHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m,
    MonadLog m
  ) =>
  TopicId ->
  MessageId ->
  m BS.ByteString
getProofHandler tId mId = do
  stateR <- asks appStateR
  state <- liftIO . readMVar $ stateR
  let da = state.bridgeState.bridgedSkyDa
      maybeRmdProof = runIdentity $ getSkyDataProofH (tId, mId) da :: Maybe (Hash, SkyDataProofH)
  case maybeRmdProof of
    Nothing -> do
      let errMsg = "Message with id: " <> show (toInt tId) <> " in topic with id: " <> show (toInt tId) <> " not found"
      logAttention_ $ pack errMsg
      throwError $ DaError errMsg
    Just (_rmd, proof) ->
      pure . fromBuiltin . toByteString $ proof
