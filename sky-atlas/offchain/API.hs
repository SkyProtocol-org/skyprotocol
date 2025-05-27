{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module API (API, api, server, startApp, AppConfig, APIError) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as BS
import Servant
import DA
import Log

type HealthAPI = "health" :> Get '[JSON] Text
type BridgeAPI = "bridge" :>
    ("read" :> Get '[JSON] Text
        :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text)
type TopicAPI = "topic" :>
    ("create" :> ReqBody '[JSON] Text :> Post '[JSON] Text
        :<|> "read" :> Get '[JSON] Text
        :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text)
type API = HealthAPI :<|> BridgeAPI :<|> TopicAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = healthServer :<|> bridgeServer :<|> topicServer

healthServer :: ServerT HealthAPI AppM
healthServer = do
  pure "OK"

bridgeServer :: ServerT BridgeAPI AppM
bridgeServer = readServer :<|> updateServer
    where
        readServer = pure "Read"
        updateServer = pure "Updated"

topicServer :: ServerT TopicAPI AppM
topicServer = createServer :<|> readServer :<|> updateServer
    where
        createServer = pure "Created"
        readServer = pure "Read"
        updateServer = pure "Updated"

data APIError = APIError String deriving (Show, Eq, Generic)
instance ToJSON APIError
instance FromJSON APIError

data AppConfig = AppConfig
  { configPort :: Int
  , configLogLevel :: Maybe String
  }

data AppEnv r = AppEnv
  { appConfig :: AppConfig,
    daData :: SkyDa r,
    logger :: Logger
  }

type AppM r = ReaderT (AppEnv r) (LogT (ExceptT APIError Handler))

startApp :: AppEnv r -> IO ()
startApp env = do
    let config = appConfig env
    run (configPort config) (serve api (hoistServer api (convertAppM env (logger config)) server))

convertAppM :: AppEnv r -> Logger -> AppM r a -> Handler a
convertAppM env logger appM = runReaderT (runLogT "api" logger defaultLogLevel (runExceptT appM)) env >>= either (throwError . toServantErr) pure

toServantErr :: APIError -> ServerError
toServantErr (APIError msg) = err500 { errBody = BS.pack msg }
