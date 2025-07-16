module API.Bridge.Contracts where

import API.SkyMintingPolicy
import Common.Crypto (Hash)
import Contract.SkyBridge
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))

mkMintingSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Amount to mint
  Maybe Integer ->
  GYTokenName ->
  -- | Minting policy signer
  GYPubKeyHash ->
  -- | Top hash
  Hash ->
  m (GYTxSkeleton 'PlutusV2)
mkMintingSkeleton amount tokenName mintSigner topHash = do
  let bridgeNFTDatum = BridgeNFTDatum topHash
      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus mintSigner
      skyToken = GYToken (mintingPolicyId skyPolicy) tokenName
      -- should be at least 1
      amt = fromMaybe 1 amount
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bvAddr <- bridgeValidatorAddress $ BridgeParams curSym

  -- skeleton for minting transaction
  pure $
    mustMint @'PlutusV2 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName amt
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = bvAddr,
            gyTxOutValue = valueSingleton skyToken amt,
            gyTxOutDatum = Just (datumFromPlutusData bridgeNFTDatum, GYTxOutUseInlineDatum),
            gyTxOutRefS = Nothing
          }
      <> mustBeSignedBy mintSigner
