module Transaction.Bridge where

import Common.Crypto (Hash)
import Contract.Bridge
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types

mkMintingSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Token name
  GYTokenName ->
  -- | Token to send
  GYAssetClass ->
  -- | Minting policy
  GYScript 'PlutusV3 ->
  -- | Top hash
  Hash ->
  -- | Addr of the bridge validator
  GYAddress ->
  -- | Minting policy signer
  GYPubKeyHash ->
  m (GYTxSkeleton 'PlutusV3)
mkMintingSkeleton tokenName skyToken skyPolicy topHash bvAddr mintSigner = do
  let bridgeNFTDatum = BridgeNFTDatum topHash

  -- skeleton for minting transaction
  pure $
    mustMint @'PlutusV3 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName 1
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
  GYScript 'PlutusV3 ->
  -- | Bridge ref
  GYTxOutRef ->
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
  m (GYTxSkeleton 'PlutusV3)
mkUpdateBridgeSkeleton validator bridgeRef newDatum redeemer skyToken bvAddr signer =
  pure $
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
