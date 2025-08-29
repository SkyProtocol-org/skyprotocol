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
    { create = asks appAdmin >>= createBridgeHandler,
      read = asks appAdmin >>= readBridgeHandler . cuserAddressPubKeyHash,
      update = asks appAdmin >>= updateBridgeHandler
    }
