{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module API (API, api, server, startApp, testEnv, AppEnv (..), AppConfig (..), APIError (..)) where

import API.Bridge
import API.Topic
import API.Types
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text (Text)
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Network.Wai.Handler.Warp (run)
import Servant
import Common (Bytes4, SkyDa, HashRef, computeHash, ofHex, derivePubKey, MultiSigPubKey (..), initDa, UInt16 (..))
import Control.Monad.Identity
import Data.IORef
import Log

type HealthAPI = "health" :> Get '[JSON] Text

healthServer :: ServerT HealthAPI AppM
healthServer = do
  pure "OK"

type API = HealthAPI :<|> BridgeAPI :<|> TopicAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = healthServer :<|> bridgeServer :<|> topicServer

convertAppM :: AppEnv -> Logger -> AppM a -> Handler a
convertAppM env logger appM = runExceptT (runLogT "api" logger defaultLogLevel (runReaderT appM env)) >>= either (throwError . toServantErr) pure

toServantErr :: APIError -> ServerError
toServantErr (APIError msg) = err500 {errBody = BS.pack msg}

startApp :: AppEnv -> IO ()
startApp env = do
  let config = appConfig env
  run (configPort config) (serve api (hoistServer api (convertAppM env (logger env)) server))

testEnv :: Logger -> IO AppEnv
testEnv logger = do
  config <- loadYamlSettings ["config/local-test.yaml"] [] requireEnv
  let daSchema = computeHash (ofHex "deadbeef" :: Bytes4)

      sk1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
      sk2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"
      pk1 = derivePubKey sk1
      pk2 = derivePubKey sk2
      committee = MultiSigPubKey ([pk1, pk2], UInt16 2)

      daData = runIdentity $ initDa daSchema committee :: SkyDa HashRef

  dataRef <- newIORef daData
  pure $ AppEnv { appConfig = config, daData = dataRef, ..}
