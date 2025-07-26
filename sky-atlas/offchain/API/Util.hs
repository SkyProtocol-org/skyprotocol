module API.Util (UtilAPI, utilServer) where

import API.Types
import API.Util.Contracts
import App
import Control.Monad.Reader
import Data.Text (pack)
import GeniusYield.TxBuilder
import Log
import Servant

-- TODO: better descriptions
-- TODO: consider making this protected!
type UtilAPI =
  Summary "Utility API for debugging and testing purposes. Not for the end user!"
    :> "util"
    :> ( "split_utxo"
           :> Description "Utility EndPoint to split one utxo into two with specified amounts"
           :> ReqBody '[JSON] CreateCollateralRequest
           :> Post '[JSON] ()
       )

utilServer :: ServerT UtilAPI AppM
utilServer = createCollateralH
  where
    createCollateralH CreateCollateralRequest {..} = do
      AppEnv {..} <- ask

      -- NOTE: we need collateral here, so if user haven't provided one - we create one ourselves
      -- NOTE: we need collateral here, so if user haven't provided one - we create one ourselves
      (wallet, amt) <- do
        mC <- runQuery $ getCollateral' (cuserAddress appAdmin) 10
        case mC of
          Nothing -> throwError $ APIError "Can't find utxo for collateral"
          Just c -> pure c
      logTrace_ $ "Found wallet utxo: " <> pack (show wallet)
      logTrace_ $ "Wallet ada balance: " <> pack (show amt)

      logTrace_ "Constructing body for the collateral request"
      body <-
        runBuilder [clrAddr] clrAddr Nothing $
          mkCreateCollateralSkeleton clrAddr wallet clrFunds clrCollateral clrAddrPubKey

      logTrace_ "Signing and submitting collateral body"
      tId <- runGY clrSigningKey Nothing [clrAddr] clrAddr Nothing $ pure body

      logTrace_ $ "Transaction id: " <> pack (show tId)
