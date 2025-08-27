module API.Bridge (BridgeApi (..), bridgeServer) where

import App
import Data.Text (Text)
import GHC.Generics (Generic)
import GeniusYield.Types
import Handler.Bridge
import Servant
import Servant.Server.Generic

-- TODO: better descriptions
data BridgeApi mode = BridgeApi
  { create ::
      mode
        :- "create"
          :> Description "Mint token and create bridge"
          :> Post '[JSON] GYTxId,
    read ::
      mode
        :- "read"
          :> Description "Read current datum from bridge"
          :> Get '[JSON] Text,
    update ::
      mode
        :- "update"
          :> Description "Update bridge datum"
          :> Post '[JSON] GYTxId
  }
  deriving stock (Generic)

bridgeServer :: BridgeApi (AsServerT AppM)
bridgeServer =
  BridgeApi
    { create = createBridgeH,
      read = readBridgeH,
      update = updateBridgeH
    }
  where
    createBridgeH = do
      AppEnv {..} <- ask
      createBridgeHandler appAdmin
    readBridgeH = do
      AppEnv {..} <- ask
      readBridgeHandler $ cuserAddressPubKeyHash appAdmin
    updateBridgeH = do
      AppEnv {..} <- ask
      updateBridgeHandler appAdmin
