module API.Util (UtilAPI, utilServer) where

import API.Types
import API.Util.Contracts
import App
import Control.Monad.Reader
import Data.Foldable (maximumBy)
import Data.Text (pack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import Servant

type UtilAPI =
  "util"
    :> ( "create_collateral" :> ReqBody '[JSON] CreateCollateralRequest :> Post '[JSON] ()
       )

utilServer :: ServerT UtilAPI AppM
utilServer = createCollateralH
  where
    createCollateralH CreateCollateralRequest {..} = do
      AppEnv {..} <- ask

      -- NOTE: we need collateral here, so if user haven't provided one - we create one ourselves
      wallet <- do
        logTrace_ "Creating utxos for collateral"
        utxos' <- runQuery $ utxosAtAddress (cuserAddress appAdmin) Nothing
        let utxos = utxosToList utxos'
        logTrace_ $ "Found utxos: " <> pack (show utxos)
        if not $ null utxos
          then pure $ maximumBy (\v1 v2 -> compare (valueAda $ utxoValue v1) (valueAda $ utxoValue v2)) utxos
          else throwError $ APIError "Can't find utxo"
      logTrace_ $ "Found wallet utxo: " <> pack (show wallet)
      logTrace_ $ "Wallet ada balance: " <> pack (show $ valueAda $ utxoValue wallet)

      logTrace_ "Constructing body for the collateral request"
      body <-
        runBuilder [clrAddr] clrAddr Nothing $
          mkCreateCollateralSkeleton clrAddr (utxoRef wallet) clrFunds clrCollateral clrAddrPubKey

      logTrace_ "Signing and submitting collateral body"
      tId <- runGY clrSigningKey Nothing [clrAddr] clrAddr Nothing $ pure body

      logTrace_ $ "Transaction id: " <> pack (show tId)
