module API
  ( SkyApi (..),
    server,
    app,
    User (..),
    UserDb (..),
  )
where

import API.Bounty
import API.Bridge
import API.Da
import API.Types
import API.Util
import App
import Control.Concurrent.STM (TVar)
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.OpenApi
import Servant.Server.Generic
import Servant.Swagger.UI

data SkyApi mode = SkyApi
  { health :: mode :- "health" :> Description "Health EndPoint" :> Get '[JSON] Text,
    bridge ::
      mode
        :- Summary "Part of the API designed to interact with the bridge"
          :> "bridge"
          :> NamedRoutes BridgeApi,
    bounty ::
      mode
        :- Summary "Part of the API to offer and claim a bounty"
          :> "bounty"
          :> NamedRoutes BountyApi,
    da ::
      mode
        :- Summary "Part of the API designed to interact with DA"
          :> "da"
          :> NamedRoutes DaApi,
    util ::
      mode
        :- Summary "Part of the API with utility functions"
          :> "util"
          :> NamedRoutes UtilApi
  }
  deriving stock (Generic)

skyApiSwagger :: OpenApi
skyApiSwagger = toOpenApi $ Proxy @(ToServantApi SkyApi)

data Api mode = Api
  { skyApi :: mode :- NamedRoutes SkyApi,
    docApi :: mode :- SwaggerSchemaUI "swagger-ui" "swagger.json"
  }
  deriving (Generic)

skyServer :: SkyApi (AsServerT AppM)
skyServer =
  SkyApi
    { health = pure "OK",
      bridge = bridgeServer,
      bounty = bountyServer,
      da = daServer,
      util = utilServer
    }

apiServer :: Api (AsServerT AppM)
apiServer =
  Api
    { skyApi = skyServer,
      docApi = swaggerSchemaUIServerT skyApiSwagger
    }

appCtx :: TVar UserDb -> Context (BasicAuthCheck User ': '[])
appCtx userDb = authCheck userDb :. EmptyContext

app :: AppEnv -> Application
app env = genericServeTWithContext (nt env) apiServer (appCtx $ appUsers env)
