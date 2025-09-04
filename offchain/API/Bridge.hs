module API.Bridge (BridgeApi (..), bridgeServer) where

import App
import Control.Monad.Reader
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
    { create = createBridgeApiHandler,
      read = asks appAdmin >>= readBridgeHandler . cuserAddressPubKeyHash,
      update = updateBridgeApiHandler
    }
  where
    createBridgeApiHandler = do
      AppEnv {..} <- ask
      skeleton <- createBridgeHandler appAdmin
      buildAndRunGY
        (cuserSigningKey appAdmin)
        Nothing
        [cuserAddress appAdmin]
        (cuserAddress appAdmin)
        Nothing
        $ pure skeleton
    updateBridgeApiHandler = do
      AppEnv {..} <- ask
      skeleton <- updateBridgeHandler appAdmin
      buildAndRunGY
        (cuserSigningKey appAdmin)
        Nothing
        [cuserAddress appAdmin]
        (cuserAddress appAdmin)
        Nothing
        $ pure skeleton

-- -- update the bridged state after we update the bridge
-- liftIO $ modifyMVar_ appStateW $ \state' -> do
--   let newBridgeState = state'.bridgeState {bridgedSkyDa = da}
--       newState = state' {bridgeState = newBridgeState}
--   modifyMVar_ appStateR . const . return $ newState
--   pure newState
