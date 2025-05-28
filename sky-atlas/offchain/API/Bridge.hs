module API.Bridge (BridgeAPI, bridgeServer) where

import API.Types
import Data.Text
import Servant

type BridgeAPI =
  "bridge"
    :> ( "read" :> Get '[JSON] Text
           :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
       )

bridgeServer :: ServerT BridgeAPI AppM
bridgeServer = readBridge :<|> updateBridge
  where
    readBridge = throwError $ APIError "Unimplemented"
    updateBridge _ = throwError $ APIError "Unimplemented"
