module API.Bounty.Contracts where

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
