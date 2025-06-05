module API.Topic (TopicAPI, topicServer) where

import API.Types
import Common as C
import Common.OffChain ()
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (asks)
import Data.ByteString qualified as BS
import Data.IORef (readIORef, writeIORef)
import Data.Text
import GHC.IO.Exception
import Log
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant

type TopicAPI =
  "topic"
    :> ( "create" :> Post '[JSON] TopicId
           :<|> "read" :> Capture "topic_id" TopicId :> Capture "message_id" MessageId :> Get '[OctetStream] BS.ByteString
           :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
       )

topicServer :: ServerT TopicAPI AppM
topicServer = createTopic :<|> readTopic :<|> updateTopic
  where
    createTopic = do
      stateW <- asks appStateW
      stateR <- asks appStateW
      tId <- liftIO . modifyMVar stateW $ \ state -> do
        let da = view (blockState . skyDa) $ state
        let (maybeTopicId, newDa) = runIdentity $ insertTopic (computeHash (ofHex "1ea7f00d" :: Bytes4)) da
        case maybeTopicId of
          Nothing -> throwError $ userError "Can't add topic"
          Just tId -> do
            let newState = set (blockState . skyDa) newDa state
            modifyMVar_ stateR . const . return $ newState
            pure (newState, tId)
      logInfo_ $ "Created topic with id " <> pack (show $ toInt tId)
      return tId
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
    updateTopic _ = throwError $ APIError "Not implemented"
    builtinByteStringToByteString (BuiltinByteString b) = b
