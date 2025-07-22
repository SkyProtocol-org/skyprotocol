module API
  ( API,
    api,
    server,
    app,
    User (..),
  )
where

import API.Bounty
import API.Bridge
import API.Topic
import API.Types
import App
import Data.Text (Text)
import Servant

type HealthAPI = "health" :> Get '[JSON] Text

healthServer :: ServerT HealthAPI AppM
healthServer = pure "OK"

type API = HealthAPI :<|> BridgeAPI :<|> BountyAPI :<|> TopicAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = healthServer :<|> bridgeServer :<|> bountyServer :<|> topicServer

appCtx :: Context (BasicAuthCheck User ': '[])
appCtx = authCheck :. EmptyContext

app :: AppEnv -> Application
app env = serveWithContext api appCtx $ hoistServerWithContext api (Proxy @(BasicAuthCheck User ': '[])) (nt env) server
