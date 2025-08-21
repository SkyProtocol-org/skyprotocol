module API.Da (DaApi (..), PublicDaApi (..), ProtectedDaApi (..), daServer) where

import API.Types
import App
import Common as C
import Common.OffChain ()
import Contract.DaH
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import GHC.Generics (Generic)
import Handler.Da
import Log
import PlutusLedgerApi.Common (fromBuiltin, toBuiltin)
import Servant
import Servant.Server.Generic
import Utils

-- TODO: better descriptions
data DaApi mode = DaApi
  { public ::
      mode
        :- Summary "Public part of the DA API"
          :> NamedRoutes PublicDaApi,
    protected ::
      mode
        :- Summary "Private part of the DA API"
          :> BasicAuth "sky_topic_realm" User
          :> NamedRoutes ProtectedDaApi
  }
  deriving stock (Generic)

-- TODO: better descriptions
data PublicDaApi mode = PublicDaApi
  { readMessage ::
      mode
        :- "read_message"
          :> Description "Returns content of the message"
          :> Capture "topic_id" TopicId
          :> Capture "message_id" MessageId
          :> Get '[OctetStream] RawBytes,
    getProof ::
      mode
        :- "get_proof"
          :> Description "Returns proof of inclusion for given message"
          :> Capture "topic_id" TopicId
          :> Capture "message_id" MessageId
          :> Get '[OctetStream] ProofBytes, -- TODO: have error 404 or whatever with JSON (or binary?) if problem appears, see https://docs.servant.dev/en/latest/cookbook/multiverb/MultiVerb.html also take an optional argument for the height of the (to be) bridged DA.
    readMessageWithTimeStamp ::
      mode
        :- "read_message_timestamp"
          :> Description "Returns timestamp + content of the message"
          :> Capture "topic_id" TopicId
          :> Capture "message_id" MessageId
          :> Get '[OctetStream] RawBytes
  }
  deriving stock (Generic)

-- TODO: better descriptions
data ProtectedDaApi mode = ProtectedDaApi
  { createTopic ::
      mode
        :- "create_topic"
          :> Description "Create new topic"
          :> Post '[JSON] TopicId,
    publishMessage ::
      mode
        :- "publish_message"
          :> Description "Publish message at topic_id"
          :> Capture "topic_id" TopicId
          :> ReqBody '[OctetStream] RawBytes
          :> Post '[JSON] (MessageId, Hash)
  }
  deriving stock (Generic)

daServer :: DaApi (AsServerT AppM)
daServer =
  DaApi
    { public = publicServer,
      protected = protectedServer
    }
  where
    publicServer =
      PublicDaApi
        { readMessage = readTopicH,
          getProof = getProofH,
          readMessageWithTimeStamp = readMessageH
        }
    protectedServer u =
      ProtectedDaApi
        { createTopic = createTopicH u,
          publishMessage = publishMessageH u
        }

readTopicH :: TopicId -> MessageId -> AppM RawBytes
readTopicH tId mId = do
  RawBytes <$> readTopicHandler tId mId

createTopicH :: (MonadLog m, MonadReader AppEnv m, MonadIO m) => User -> m TopicId
createTopicH _user = do
  createTopicHandler

publishMessageH :: User -> TopicId -> RawBytes -> AppM (MessageId, Hash)
publishMessageH _user topicId (RawBytes msgBody) = do
  publishMessageHandler topicId msgBody

getProofH :: TopicId -> MessageId -> AppM ProofBytes
getProofH topicId messageId = do
  ProofBytes <$> getProofHandler topicId messageId

-- TODO: Why do we need this?
readMessageH :: TopicId -> MessageId -> AppM RawBytes
readMessageH topicId msgId = do
  stateR <- asks appStateR
  state <- liftIO . readMVar $ stateR
  let da = view (blockState . skyDa) state
      maybeMessageEntry = runIdentity $ getMessage topicId msgId da
  case maybeMessageEntry of
    Nothing -> throwError $ APIError "readMessage failed"
    Just (rmd, rd) -> do
      MessageMetaData time <- unwrap rmd
      message <- unwrap rd
      return . RawBytes . fromBuiltin . toByteString $ (time, message)
