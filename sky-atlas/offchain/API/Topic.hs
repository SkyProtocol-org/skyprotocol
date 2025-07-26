module API.Topic (TopicAPI, topicServer) where

import API.Types
import App
import Common as C
import Common.OffChain ()
import Contract.DaH
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString qualified as BS
import Data.Fixed
import Data.Time
import Data.Time.Clock qualified as DTC
import Data.Time.Clock.POSIX qualified as DTCP
import Log
import PlutusLedgerApi.V1.Time qualified as T (POSIXTime (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant

-- TODO: better descriptions
-- TODO: better naming? This is basically interactions with the DA, not only topics?
type TopicAPI =
  Summary "Part of the API designed to interact with DA"
    :> "topic"
    :> (PublicTopicAPI :<|> (BasicAuth "sky_topic_realm" User :> ProtectedTopicAPI))

-- TODO: better descriptions
type PublicTopicAPI =
  Summary "Public part of the DA API"
    :> ( "read"
           :> Description "Returns content of the message"
           :> Capture "topic_id" TopicId
           :> Capture "message_id" MessageId
           :> Get '[OctetStream] BS.ByteString
       )
    :<|> "get_proof"
      :> Description "Returns proof of inclusion for given message"
      :> Capture "topic_id" TopicId
      :> Capture "message_id" MessageId
      :> Get '[OctetStream] BS.ByteString -- TODO: have error 404 or whatever with JSON (or binary?) if problem appears, see https://docs.servant.dev/en/latest/cookbook/multiverb/MultiVerb.html also take an optional argument for the height of the (to be) bridged DA.
    :<|> "read_message"
      :> Description "Returns timestamp + content of the message"
      :> Capture "topic_id" TopicId
      :> Capture "message_id" MessageId
      :> Get '[OctetStream] BS.ByteString

-- TODO: better descriptions
type ProtectedTopicAPI =
  Summary "Private part of the DA API"
    :> ( "create"
           :> Description "Create new topic"
           :> Post '[JSON] TopicId
       )
    :<|> "publish_message"
      :> Description "Publish message at topic_id"
      :> Capture "topic_id" TopicId
      :> ReqBody '[OctetStream] BS.ByteString
      :> Post '[JSON] MessageId

topicServer :: ServerT TopicAPI AppM
topicServer = unprotectedServer :<|> protectedServer
  where
    unprotectedServer = readTopic :<|> getProof :<|> readMessage
    protectedServer u = createTopic u :<|> publishMessage u

readTopic :: TopicId -> MessageId -> AppM BS.ByteString
readTopic tId mId = do
  stateR <- asks appStateR
  state <- liftIO $ readMVar stateR
  let SkyDa {..} = view (blockState . skyDa) state
  maybeTopic <- C.lookup tId =<< unwrap skyTopicTrie
  case maybeTopic of
    Nothing -> throwError . APIError $ "Can't find topic with id " <> show (toInt tId)
    Just (_tMeta, messageTrie) -> do
      maybeMessage <- C.lookup mId =<< unwrap messageTrie
      case maybeMessage of
        Nothing -> throwError . APIError $ "Can't find message with id " <> show (toInt mId)
        Just (_mMeta, mData) -> builtinByteStringToByteString <$> unwrap mData

createTopic :: (MonadLog m, MonadReader AppEnv m, MonadIO m) => User -> m TopicId
createTopic _user = do
  stateW <- asks appStateW
  stateR <- asks appStateR
  topicId <- liftIO . modifyMVar stateW $ \state -> do
    let da = view (blockState . skyDa) state
    let (newDa, maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da
    case maybeTopicId of
      Nothing -> throwError $ userError "Can't add topic"
      Just topicId -> do
        let newState = set (blockState . skyDa) newDa state
        modifyMVar_ stateR . const . return $ newState
        pure (newState, topicId)
  logTrace "Created topic " topicId
  return topicId

utcTimeEpoch :: DTC.UTCTime
utcTimeEpoch = DTCP.posixSecondsToUTCTime 0

-- Get current time in Plutus-friendly format
currentPOSIXTime :: IO T.POSIXTime
currentPOSIXTime = do
  utcTime <- getCurrentTime
  let (MkFixed nominalDiffTime) = nominalDiffTimeToSeconds $ diffUTCTime utcTime utcTimeEpoch
  return . T.POSIXTime $ nominalDiffTime `div` 1000000000

publishMessage :: User -> TopicId -> BS.ByteString -> AppM MessageId
publishMessage _user topicId msgBody = do
  stateW <- asks appStateW
  stateR <- asks appStateR
  maybeMessageId <- liftIO . modifyMVar stateW $ \state -> do
    let da = view (blockState . skyDa) state
    timestamp <- currentPOSIXTime
    let (newDa, maybeMessageId) = runIdentity $ C.insertMessage timestamp (BuiltinByteString msgBody) topicId da
    let newState = (set (blockState . skyDa) newDa state, maybeMessageId)
    modifyMVar_ stateR . const . return $ fst newState
    return newState
  case maybeMessageId of
    Nothing -> throwError $ APIError "publishMessage failed"
    Just messageId -> do
      logTrace "Published message" messageId
      return messageId

getProof :: TopicId -> MessageId -> AppM BS.ByteString
getProof topicId messageId = do
  stateR <- asks appStateR
  state <- liftIO . readMVar $ stateR
  let da = view (bridgeState . bridgedSkyDa) state
  let maybeRmdProof = runIdentity $ getSkyDataProofH (topicId, messageId) da :: Maybe (Hash, SkyDataProofH)
  case maybeRmdProof of
    Nothing -> throwError $ APIError "Message not found. Maybe wait for bridge update?"
    Just (rmd, proof) ->
      return . builtinByteStringToByteString . toByteString $ (rmd, proof)

-- TODO: Why do we need this?
readMessage :: TopicId -> MessageId -> AppM BS.ByteString
readMessage topicId msgId = do
  stateR <- asks appStateR
  state <- liftIO . readMVar $ stateR
  let da = view (blockState . skyDa) state
  let maybeMessageEntry = runIdentity $ getMessage topicId msgId da
  case maybeMessageEntry of
    Nothing -> throwError $ APIError "readMessage failed"
    Just (rmd, rd) -> do
      MessageMetaData time <- unwrap rmd
      message <- unwrap rd
      return . builtinByteStringToByteString . toByteString $ (time, message)
