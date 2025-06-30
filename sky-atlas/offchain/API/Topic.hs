module API.Topic (TopicAPI, topicServer) where

import API.Types
import Common as C
import Common.OffChain ()
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString qualified as BS
import Data.Fixed
import Data.Text
import Data.Time
import Data.Time.Clock qualified as DTC
import Data.Time.Clock.POSIX qualified as DTCP
import GHC.IO.Exception
import Log
-- import PlutusTx.Prelude qualified as P
import PlutusLedgerApi.V1.Time qualified as T (POSIXTime (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant

type TopicAPI =
  "topic" :> (PublicTopicAPI :<|> (BasicAuth "sky_topic_realm" User :> ProtectedTopicAPI))

type PublicTopicAPI =
  ( "read" :> Capture "topic_id" TopicId :> Capture "message_id" MessageId :> Get '[OctetStream] BS.ByteString
  :<|> "get_proof" :> Capture "topic_id" TopicId :> Capture "message_id" MessageId :> Get '[OctetStream] BS.ByteString -- TODO: have error 404 or whatever with JSON (or binary?) if problem appears, see https://docs.servant.dev/en/latest/cookbook/multiverb/MultiVerb.html
  )

type ProtectedTopicAPI =
  ( "create" :> Post '[JSON] TopicId
  -- :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
  -- :<|> "add_message" :> Capture "topic_id" TopicId :> ReqBody '[OctetStream] BS.ByteString :> Post '[JSON] MessageId
  )

topicServer :: ServerT TopicAPI AppM
topicServer = (readTopic :<|> getProof) :<|> (createTopic) -- :<|> addMessage)

readTopic :: TopicId -> MessageId -> AppM BS.ByteString
readTopic tId mId = do
  stateR <- asks appStateR
  state <- liftIO $ readMVar stateR
  let SkyDa {..} = view (blockState . skyDa) $ state
  maybeTopic <- C.lookup tId =<< unwrap skyTopicTrie
  case maybeTopic of
    Nothing -> throwError . APIError $ "Can't find topic with id " <> show (toInt tId)
    Just (_tMeta, messageTrie) -> do
      maybeMessage <- C.lookup mId =<< unwrap messageTrie
      case maybeMessage of
        Nothing -> throwError . APIError $ "Can't find message with id " <> show (toInt mId)
        Just (_mMeta, mData) -> builtinByteStringToByteString <$> unwrap mData

createTopic :: User -> AppM TopicId
createTopic _ = do
  stateW <- asks appStateW
  stateR <- asks appStateR
  topicId <- liftIO . modifyMVar stateW $ \ state -> do
    let da = view (blockState . skyDa) $ state
    let (newDa, maybeTopicId) = runIdentity $ insertTopic (computeHash (ofHex "1ea7f00d" :: Bytes4)) da
    case maybeTopicId of
      Nothing -> throwError $ userError "Can't add topic"
      Just topicId -> do
        let newState = set (blockState . skyDa) newDa state
        modifyMVar_ stateR . const . return $ newState
        pure (newState, topicId)
  logInfo_ $ "Created topic with id " <> pack (show $ toInt topicId)
  return topicId

-- updateTopic _ = throwError $ APIError "Not implemented"


utcTimeEpoch :: DTC.UTCTime
utcTimeEpoch = DTCP.posixSecondsToUTCTime 0

-- Get current time in Plutus-friendly format
currentPOSIXTime :: IO T.POSIXTime
currentPOSIXTime = do
  utcTime <- getCurrentTime
  let (MkFixed nominalDiffTime) = nominalDiffTimeToSeconds $ diffUTCTime utcTime utcTimeEpoch
  return . T.POSIXTime $ nominalDiffTime `div` 1000000000

addMessage :: User -> TopicId -> BS.ByteString -> AppM MessageId
addMessage User {..} topicId msgBody = do
  stateW <- asks appStateW
  maybeMessageId <- liftIO . modifyMVar stateW $ \ state -> do
    let da = view (blockState . skyDa) $ state
    timestamp <- currentPOSIXTime
    let (newDa, maybeMessageId) = runIdentity $ C.insertMessage userPubKey timestamp (BuiltinByteString msgBody) topicId da
    return (set (blockState . skyDa) newDa state, maybeMessageId)
  case maybeMessageId of
    Nothing -> throwError $ APIError "addMessage failed"
    Just messageId -> return messageId

getProof :: TopicId -> MessageId -> AppM BS.ByteString
getProof topicId messageId =
  -- TODO: get the tophash and skyDa from the Bridge, build a proof relative to THAT
  -- which means we maintain in the AppState not just the current Da, but the current bridged Da
  throwError $ APIError "Not implemented"
