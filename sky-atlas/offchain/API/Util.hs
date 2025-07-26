module API.Util (UtilApi (..), utilServer) where

import API.Types
import API.Util.Contracts
import App
import Control.Monad.Reader
import Data.Text (pack)
import GHC.Generics (Generic)
import GeniusYield.TxBuilder
import Log
import Servant
import Servant.Server.Generic

-- TODO: better descriptions
-- TODO: consider making this protected!
data UtilApi mode = UtilApi
  { splitUtxo ::
      mode
        :- "split_utxo"
          :> Description "Utility EndPoint to split one utxo into two with specified amounts"
          :> ReqBody '[JSON] CreateCollateralRequest
          :> Post '[JSON] ()
  }
  deriving stock (Generic)

utilServer :: UtilApi (AsServerT AppM)
utilServer = UtilApi {splitUtxo = splitUtxoH}
  where
    splitUtxoH CreateCollateralRequest {..} = do
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
