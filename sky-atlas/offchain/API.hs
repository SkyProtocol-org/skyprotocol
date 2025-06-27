{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( API
  , api
  , server
  , app
  , testEnv
  , convertAppM
  , AppEnv (..)
  , AppConfig (..)
  , AppError (..)
  ) where

import API.Bridge
import API.Topic
import API.Types
import Common (Bytes4, HashRef, MultiSigPubKey (..), SkyDa, UInt16 (..), computeHash, derivePubKey, initDa, ofHex)
import Control.Concurrent.MVar
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text (Text)
import Log
import Servant
import GeniusYield.Types (GYProviders)

type HealthAPI = "health" :> Get '[JSON] Text

healthServer :: ServerT HealthAPI AppM
healthServer = pure "OK"

type API = HealthAPI :<|> BridgeAPI :<|> TopicAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = healthServer :<|> bridgeServer :<|> topicServer

convertAppM :: AppEnv -> Logger -> AppM a -> Handler a
convertAppM env logger appM = runExceptT (runLogT "api" logger defaultLogLevel (runReaderT appM env)) >>= either (throwError . toServantErr) pure

toServantErr :: AppError -> ServerError
toServantErr (APIError msg) = err500 {errBody = BS.pack msg}
-- toServantErr _ = err500 {errBody = "ISE ???"}

{- TODO: move that to a separate file about the test environment.
   Also, do not use a known key for the official shared test environment, but keys from a config file -}

testSecKey1 :: SecKey
testSecKey1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
testSecKey2 :: SecKey
testSecKey2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"
testPubKey1 :: PubKey
testPubKey1 = derivePubKey testSecKey1
testPubKey2 :: PubKey
testPubKey2 = derivePubKey testSecKey2

testEnv :: AppConfig -> Logger -> GYProviders -> IO AppEnv
testEnv appConfig logger appProviders = do
  let daSchema = computeHash (ofHex "deadbeef" :: Bytes4)
      committee = MultiSigPubKey ([testPubKey1, testPubKey2], UInt16 2)
      _skyDa = runIdentity $ initDa daSchema committee :: SkyDa HashRef
      _blockState = BlockState { _skyDa,
                             _erasureCoding = (), _superTopic = (), _subTopics = (),
                             _publisherPayments = () }
      appState = AppState { _blockState,
                            _oldBlockQueue = (),
                            _partialSignatures = (),
                            _bridgeState = (),
                            _stake = (),
                            _peers = (),
                            _clients = (),
                            _subscriberPayments = (),
                            _auctions = (),
                            _longTermStorage = () }
  appStateW <- newMVar appState
  appStateR <- newMVar appState
  pure $ AppEnv {..}

ctx :: Context '[BasicAuthCheck User]
ctx = authCheck :. EmptyContext

app :: AppEnv -> Application
app env = serveWithContext api ctx $ hoistServer api (convertAppM env (logger env)) server

authCheck :: BasicAuthCheck User
authCheck = basicAuthCheck $ \ (BasicAuthData email password) ->
  if email == "skyAdmin" && password == "1234" then
    return $ Authorized (User email testPubKey1)
  else
    return $ Unauthorized
