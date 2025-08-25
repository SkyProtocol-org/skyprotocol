module API.Util (UtilApi (..), utilServer) where

import App
import Control.Monad
import Control.Monad.Reader
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Servant
import Servant.Server.Generic

data UtilApi mode = UtilApi
  { transferFunds ::
      mode
        :- "transfer"
          :> Summary "Transfer funds between accounts"
          :> ReqBody '[JSON] (String, [(String, Integer)])
          :> Get '[JSON] [GYTxId]
  }
  deriving stock (Generic)

utilServer :: UtilApi (AsServerT AppM)
utilServer =
  UtilApi
    { transferFunds = transferFundsH
    }
  where
    transferFundsH :: (String, [(String, Integer)]) -> AppM [GYTxId]
    transferFundsH (userFrom', usersTo) = do
      userFrom <- getUserByName userFrom'
      usersToWithAmounts <- forM usersTo \(name, amount) -> (,amount) <$> getUserByName name

      utxoFrom <- do
        utxos' <- runQuery $ utxosAtAddress (cuserAddress userFrom) Nothing
        let utxos = utxosToList utxos'
        case utxos of
          (utxo : _) -> pure $ utxoRef utxo
          _ -> throwError $ APIError "Can't find utxo to draw funds from"

      bodies <- forM usersToWithAmounts \(usr, amount) -> do
        runBuilder
          [cuserAddress userFrom]
          (cuserAddress userFrom)
          Nothing
          $ mkTransferSkeleton
            utxoFrom
            (cuserAddress usr)
            amount
            (cuserAddressPubKeyHash userFrom)
      forM bodies \body -> do
        runGY
          (cuserSigningKey userFrom)
          Nothing
          [cuserAddress userFrom]
          (cuserAddress userFrom)
          Nothing
          $ pure body

getUserByName :: String -> AppM CardanoUser
getUserByName name = do
  AppEnv {..} <- ask
  case name of
    "admin" -> pure appAdmin
    "offerer" -> pure appOfferer
    "claimant" -> pure appClaimant
    _ -> throwError $ APIError $ "Can't find the user by the name " <> name

mkTransferSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Where to take funds
  GYTxOutRef ->
  -- | Where to send funds
  GYAddress ->
  -- | Amount to send
  Integer ->
  -- | Signer
  GYPubKeyHash ->
  m (GYTxSkeleton 'PlutusV3)
mkTransferSkeleton fromInput toAddr amount signer =
  pure $
    mustHaveRefInput fromInput
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = toAddr,
            gyTxOutValue = valueSingleton GYLovelace amount,
            gyTxOutDatum = Nothing,
            gyTxOutRefS = Nothing
          }
      <> mustBeSignedBy signer
