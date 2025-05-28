module API.Topic (TopicAPI, topicServer) where

import API.Types
import Common.DA
import Common.OffChain
import Data.Text
import Servant

type TopicAPI =
  "topic"
    :> ( "create" :> Post '[JSON] TopicId
           :<|> "read" :> Get '[JSON] Text
           :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
       )

topicServer :: ServerT TopicAPI AppM
topicServer = createTopic :<|> readTopic :<|> updateTopic
  where
    createTopic = throwError $ APIError "Unimplemented"
    readTopic = throwError $ APIError "Unimplemented"
    updateTopic _ = throwError $ APIError "Unimplemented"
