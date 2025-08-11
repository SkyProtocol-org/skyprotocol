module API.Bounty.Contracts where

import Contract.Bounty (ClientRedeemer)
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types

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
  m (GYTxSkeleton 'PlutusV3)
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
  -- | Deadline
  GYSlot ->
  -- | Where to get funds
  GYTxOutRef ->
  -- | Validator
  GYScript 'PlutusV3 ->
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
  m (GYTxSkeleton 'PlutusV3)
mkClaimBountySkeleton
  deadlineSlot
  bountyRef
  validator
  nftRef
  redeemer
  recipientAddr
  amt
  signer =
    pure $
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
