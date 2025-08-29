module API.Da (DaApi (..), PublicDaApi (..), ProtectedDaApi (..), daServer) where

import API.Types
import App
import Common
import GHC.Generics (Generic)
import Handler.Da
import Servant
import Servant.Server.Generic

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
          :> Get '[OctetStream] ProofBytes -- TODO: have error 404 or whatever with JSON (or binary?) if problem appears, see https://docs.servant.dev/en/latest/cookbook/multiverb/MultiVerb.html also take an optional argument for the height of the (to be) bridged DA.
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
        { readMessage = \tId mId -> RawBytes <$> readTopicHandler tId mId,
          getProof = \tId mId -> ProofBytes <$> getProofHandler tId mId
        }
    protectedServer _u =
      ProtectedDaApi
        { createTopic = createTopicHandler,
          publishMessage = \tId (RawBytes msgBody) -> publishMessageHandler tId msgBody
        }
