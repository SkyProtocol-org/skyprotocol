module API.Bridge.Contracts where

import API.SkyMintingPolicy
import Common.Crypto (Hash)
import Contract.SkyBridge
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.Common hiding (PlutusV2)
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))

mkMintingSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  GYTokenName ->
  -- | Minting policy signer
  GYPubKeyHash ->
  -- | Top hash
  Hash ->
  m (GYTxSkeleton 'PlutusV2)
mkMintingSkeleton tokenName mintSigner topHash = do
  let bridgeNFTDatum = BridgeNFTDatum topHash
      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus mintSigner
      skyToken = GYToken (mintingPolicyId skyPolicy) tokenName
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bvAddr <- bridgeValidatorAddress $ BridgeParams curSym

  -- skeleton for minting transaction
  pure $
    mustMint @'PlutusV2 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName 1
      <> mustHaveOutput
        GYTxOut
          { gyTxOutAddress = bvAddr,
            gyTxOutValue = valueSingleton skyToken 1,
            gyTxOutDatum = Just (datumFromPlutusData bridgeNFTDatum, GYTxOutUseInlineDatum),
            gyTxOutRefS = Nothing
          }
      <> mustBeSignedBy mintSigner

mkUpdateBridgeSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Validator
  GYScript 'PlutusV2 ->
  -- | Bridge ref
  GYTxOutRef ->
  -- | Old bridge Datum
  BridgeNFTDatum ->
  -- | New bridge Datum
  BridgeNFTDatum ->
  -- | Redeemer
  BridgeRedeemer ->
  -- | Token to send
  GYAssetClass ->
  -- | Addr of the bridge validator
  GYAddress ->
  -- | Signer
  GYPubKeyHash ->
  m (GYTxSkeleton 'PlutusV2)
mkUpdateBridgeSkeleton validator bridgeRef oldDatum newDatum redeemer skyToken addr signer =
  pure $
    -- bridge input
    mustHaveInput
      ( GYTxIn
          { gyTxInTxOutRef = bridgeRef,
            gyTxInWitness =
              GYTxInWitnessScript
                (GYBuildPlutusScriptInlined validator)
                Nothing -- datum can be omitted if it was inlined
                $ redeemerFromPlutus'
                $ toBuiltinData redeemer
          }
      )
      <> mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = addr,
              gyTxOutValue = valueSingleton skyToken 1,
              gyTxOutDatum = Just (datumFromPlutusData newDatum, GYTxOutUseInlineDatum),
              gyTxOutRefS = Nothing
            }
        )
      <> mustBeSignedBy signer
