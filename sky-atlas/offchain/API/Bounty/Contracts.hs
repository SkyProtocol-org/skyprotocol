module API.Bounty.Contracts where

import Contract.Bounty (ClientRedeemer)
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.Common hiding (PlutusV2)

mkSendSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Where to send
  GYAddress ->
  -- | Amount to send
  Integer ->
  -- | What asset to send
  GYAssetClass ->
  -- | Signer
  GYPubKeyHash ->
  m (GYTxSkeleton 'PlutusV2)
mkSendSkeleton addr amount assetClass signer =
  pure $
    mustHaveOutput
      GYTxOut
        { gyTxOutAddress = addr,
          gyTxOutValue = valueSingleton assetClass amount,
          gyTxOutDatum = Nothing,
          gyTxOutRefS = Nothing
        }
      <> mustBeSignedBy signer

mkClaimBountySkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Validator
  GYScript 'PlutusV2 ->
  -- | NFT ref
  GYTxOutRef ->
  -- | Redeemer
  ClientRedeemer ->
  -- | Where to send funds
  GYAddress ->
  -- | How much to send
  Integer ->
  -- | Signer
  GYPubKeyHash ->
  m (GYTxSkeleton 'PlutusV2)
mkClaimBountySkeleton validator nftRef redeemer addr amt signer = do
  pure $
    mustHaveInput
      ( GYTxIn
          { -- TODO: ref input should be nft from bridge
            gyTxInTxOutRef = nftRef,
            gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined validator) Nothing $ redeemerFromPlutus' $ toBuiltinData redeemer
          }
      )
      <> mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = addr,
              gyTxOutValue = valueSingleton GYLovelace amt,
              gyTxOutDatum = Nothing,
              gyTxOutRefS = Nothing
            }
        )
      <> mustBeSignedBy signer
