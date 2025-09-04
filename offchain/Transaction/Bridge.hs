module Transaction.Bridge where

import Contract.Bridge
import GeniusYield.TxBuilder
import GeniusYield.Types

mkMintingSkeleton ::
  -- | Token name
  GYTokenName ->
  -- | Token to send
  GYAssetClass ->
  -- | Minting policy
  GYScript 'PlutusV3 ->
  -- | Top hash
  BridgeDatum ->
  -- | Addr of the bridge validator
  GYAddress ->
  -- | Minting policy signer
  GYPubKeyHash ->
  GYTxSkeleton 'PlutusV3
mkMintingSkeleton tokenName skyToken skyPolicy bridgeDatum bvAddr mintSigner = do
  -- skeleton for minting transaction
  mustMint @'PlutusV3 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName 1
    <> mustHaveOutput
      GYTxOut
        { gyTxOutAddress = bvAddr,
          gyTxOutValue = valueSingleton skyToken 1,
          gyTxOutDatum = Just (datumFromPlutusData bridgeDatum, GYTxOutUseInlineDatum),
          gyTxOutRefS = Nothing
        }
    <> mustBeSignedBy mintSigner

mkUpdateBridgeSkeleton ::
  -- | Validator
  GYScript 'PlutusV3 ->
  -- | Bridge ref
  GYTxOutRef ->
  -- | New bridge Datum
  BridgeDatum ->
  -- | Redeemer
  BridgeRedeemer ->
  -- | Token to send
  GYAssetClass ->
  -- | Addr of the bridge validator
  GYAddress ->
  -- | Signer
  GYPubKeyHash ->
  GYTxSkeleton 'PlutusV3
mkUpdateBridgeSkeleton validator bridgeRef newDatum redeemer skyToken bvAddr signer =
  -- bridge input
  mustHaveInput
    ( GYTxIn
        { gyTxInTxOutRef = bridgeRef,
          gyTxInWitness =
            GYTxInWitnessScript
              (GYBuildPlutusScriptInlined validator)
              Nothing -- datum can be omitted if it was inlined
              $ redeemerFromPlutusData redeemer
        }
    )
    <> mustHaveOutput
      ( GYTxOut
          { gyTxOutAddress = bvAddr,
            gyTxOutValue = valueSingleton skyToken 1,
            gyTxOutDatum = Just (datumFromPlutusData newDatum, GYTxOutUseInlineDatum),
            gyTxOutRefS = Nothing
          }
      )
    <> mustBeSignedBy signer
