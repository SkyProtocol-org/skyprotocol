module API.Bridge (BridgeAPI, bridgeServer) where

import API.Bridge.Contracts
import API.Types
import App
import Common
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Data.Text
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import Servant

type BridgeAPI =
  "bridge"
    :> ( "create" :> ReqBody '[JSON] CreateBridgeRequest :> Post '[JSON] ()
           :<|> "read" :> Get '[JSON] Text
           :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
       )

bridgeServer :: ServerT BridgeAPI AppM
bridgeServer = createBridgeH :<|> readBridgeH :<|> updateBridgeH
  where
    -- TODO: create a bridged version of the skyda and store it in app state
    createBridgeH CreateBridgeRequest {..} = do
      AppEnv {..} <- ask
      state <- liftIO $ readMVar appStateR

      let SkyDa {..} = view (blockState . skyDa) state
          topHash = toByteString $ computeDigest @Blake2b_256 $ SkyDa {..}

      logTrace_ "Constructing body for the minting policy"
      body <-
        runBuilder cbrUsedAddrs cbrChangeAddr cbrCollateral $
          mkMintingSkeleton cbrAmount (configTokenName appConfig) (pubKeyHash $ cuserVerificationKey appAdmin) topHash

      logTrace_ "Signing and submitting minting policy"
      tId <- runGY (cuserSigningKey appAdmin) Nothing cbrUsedAddrs cbrChangeAddr cbrCollateral $ pure body
      logTrace_ $ "Transaction id: " <> pack (show tId)

    readBridgeH = throwError $ APIError "Unimplemented"
    updateBridgeH _ = throwError $ APIError "Unimplemented"
