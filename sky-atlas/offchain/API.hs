module API
  ( SkyApi,
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
import API.Util
import App
import Data.OpenApi
import Data.Text (Text)
import Servant
import Servant.OpenApi
import Servant.Swagger.UI

type HealthAPI = "health" :> Description "Health EndPoint" :> Get '[JSON] Text

healthServer :: ServerT HealthAPI AppM
healthServer = pure "OK"

type SkyApi = HealthAPI :<|> BridgeAPI :<|> BountyAPI :<|> TopicAPI :<|> UtilAPI

-- skyApiSwagger :: OpenApi
-- skyApiSwagger = toOpenApi $ Proxy @SkyApi

type DocApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- type API = DocApi :<|> DocApi

api :: Proxy SkyApi
api = Proxy

skyServer :: ServerT SkyApi AppM
skyServer =
  -- swaggerSchemaUIServer skyApiSwagger :<|>
  ( healthServer
      :<|> bridgeServer
      :<|> bountyServer
      :<|> topicServer
      :<|> utilServer
  )

appCtx :: Context (BasicAuthCheck User ': '[])
appCtx = authCheck :. EmptyContext

app :: AppEnv -> Application
app env = serveWithContext api appCtx $ hoistServerWithContext api (Proxy @(BasicAuthCheck User ': '[])) (nt env) skyServer
