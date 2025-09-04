module Transaction.Bounty where

import Contract.Bounty (BountyDatum, BountyRedeemer)
import GeniusYield.TxBuilder
import GeniusYield.Types

mkOfferBountySkeleton ::
  -- | Where to send
  GYAddress ->
  BountyDatum ->
  -- | Amount to send
  Integer ->
  -- | What asset to send
  GYAssetClass ->
  -- | Signer
  GYPubKeyHash ->
  GYTxSkeleton 'PlutusV3
mkOfferBountySkeleton addr datum amount assetClass signer =
  mustHaveOutput
    GYTxOut
      { gyTxOutAddress = addr,
        gyTxOutValue = valueSingleton assetClass amount,
        gyTxOutDatum = Just (datumFromPlutusData datum, GYTxOutUseInlineDatum),
        gyTxOutRefS = Nothing
      }
    <> mustBeSignedBy signer

mkClaimBountySkeleton ::
  -- | Deadline
  GYSlot ->
  -- | Where to get funds
  GYTxOutRef ->
  -- | Validator
  GYScript 'PlutusV3 ->
  -- | NFT ref
  GYTxOutRef ->
  -- | Redeemer
  BountyRedeemer ->
  -- | Where to send funds
  GYAddress ->
  -- | How much to send
  Integer ->
  -- | Signer
  GYPubKeyHash ->
  GYTxSkeleton 'PlutusV3
mkClaimBountySkeleton
  deadlineSlot
  bountyRef
  validator
  nftRef
  redeemer
  recipientAddr
  amt
  signer =
    mustHaveRefInput nftRef
      <> mustHaveInput
        ( GYTxIn
            { gyTxInTxOutRef = bountyRef,
              gyTxInWitness =
                GYTxInWitnessScript
                  (GYBuildPlutusScriptInlined validator)
                  Nothing -- datum can be omitted if it was inlined
                  $ redeemerFromPlutusData redeemer
            }
        )
      <> mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = recipientAddr,
              gyTxOutValue = valueSingleton GYLovelace amt,
              gyTxOutDatum = Nothing,
              gyTxOutRefS = Nothing
            }
        )
      <> isInvalidAfter deadlineSlot
      <> mustBeSignedBy signer
