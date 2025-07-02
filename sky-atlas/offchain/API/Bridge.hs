module API.Bridge (BridgeAPI, bridgeServer) where

import API.SkyMintingPolicy
import API.Types
import App
import Common
import Contract.SkyBridge
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx.Prelude (BuiltinByteString)
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
    createBridgeH CreateBridgeRequest {..} = do
      AppEnv {..} <- ask
      state <- liftIO $ readMVar appStateR

      let SkyDa {..} = view (blockState . skyDa) state
          topHash = toByteString $ computeDigest @Blake2b_256 $ SkyDa {..}

      body <-
        runBuilder cbrUsedAddrs cbrChangeAddr cbrCollateral $
          createBridge cbrAmount (configTokenName appConfig) (pubKeyHash $ cuserVerificationKey appAdmin) topHash

      void $ runGY (cuserSigningKey appAdmin) Nothing cbrUsedAddrs cbrChangeAddr cbrCollateral $ pure body

    readBridgeH = throwError $ APIError "Unimplemented"
    updateBridgeH _ = throwError $ APIError "Unimplemented"

createBridge ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Amount to mint
  Maybe Integer ->
  GYTokenName ->
  -- | Minting policy signer
  GYPubKeyHash ->
  -- | Top hash
  BuiltinByteString ->
  m (GYTxSkeleton 'PlutusV2)
createBridge amount tokenName mintSigner topHash = do
  let bridgeNFTDatum = BridgeNFTDatum topHash
      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus mintSigner
      skyToken = GYToken (mintingPolicyId skyPolicy) tokenName
      -- should be at least 1
      amt = fromMaybe 1 amount
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bvAddr <- bridgeValidatorAddress $ BridgeParams curSym

  -- skeleton for minting transaction
  let mintSkeleton =
        mustMint @'PlutusV2 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName amt
          <> mustHaveOutput
            ( GYTxOut
                { -- recipient is the bridge validator
                  gyTxOutAddress = bvAddr,
                  gyTxOutValue = valueSingleton skyToken amt,
                  gyTxOutDatum = Just (datumFromPlutusData bridgeNFTDatum, GYTxOutUseInlineDatum),
                  gyTxOutRefS = Nothing
                }
            )
          <> mustBeSignedBy mintSigner
  pure mintSkeleton
