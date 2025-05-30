module API.Topic (TopicAPI, topicServer) where

import API.Types
import Common as C
import Common.OffChain ()
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (asks)
import Data.IORef (readIORef, writeIORef)
import Data.Text
import Servant

type TopicAPI =
  "topic"
    :> ( "create" :> Post '[JSON] TopicId
           :<|> "read" :> Capture "topic_id" TopicId :> Capture "message_id" MessageId :> Get '[JSON] Text
           :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
       )

topicServer :: ServerT TopicAPI AppM
topicServer = createTopic :<|> readTopic :<|> updateTopic
  where
    createTopic = do
      daRef <- asks daData
      da <- liftIO $ readIORef daRef
      let (maybeTopicId, newDa) = runIdentity $ insertTopic (computeHash (ofHex "1ea7f00d" :: Bytes4)) da
      case maybeTopicId of
        Nothing -> throwError $ APIError "Can't add topic"
        Just tId -> do
          liftIO $ writeIORef daRef newDa
          pure tId
    readTopic tId mId = do
      daRef <- asks daData
      (_skyMeta, rTopicTrie) <- liftIO $ readIORef daRef
      maybeTopic <- C.lookup tId =<< unwrap rTopicTrie
      case maybeTopic of
        Nothing -> throwError $ APIError "Can't find topic"
        Just (_tMeta, messageTrie) -> do
          maybeMessage <- C.lookup mId =<< unwrap messageTrie
          case maybeMessage of
            Nothing -> throwError $ APIError "Can't find message"
            Just (_mMeta, mData) -> unwrap mData
    updateTopic _ = throwError $ APIError "Unimplemented"
