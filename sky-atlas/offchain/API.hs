{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module API (API, api, server, startApp, AppEnv (..), AppConfig (..), APIError (..)) where

import API.Bridge
import API.Topic
import API.Types
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text (Text)
import Log
import Network.Wai.Handler.Warp (run)
import Servant

type HealthAPI = "health" :> Get '[JSON] Text

type API = HealthAPI :<|> BridgeAPI -- :<|> TopicAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = healthServer :<|> bridgeServer -- :<|> topicServer

healthServer :: ServerT HealthAPI AppM
healthServer = do
  throwError $ APIError "Unimplemented"

startApp :: AppEnv -> IO ()
startApp env = do
  let config = appConfig env
  run (configPort config) (serve api (hoistServer api (convertAppM env (logger env)) server))

convertAppM :: AppEnv -> Logger -> AppM a -> Handler a
convertAppM env logger appM = runExceptT (runLogT "api" logger defaultLogLevel (runReaderT appM env)) >>= either (throwError . toServantErr) pure

toServantErr :: APIError -> ServerError
toServantErr (APIError msg) = err500 {errBody = BS.pack msg}
