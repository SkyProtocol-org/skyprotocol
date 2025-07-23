module API.Util.Contracts where

import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types

mkCreateCollateralSkeleton ::
  (HasCallStack, GYTxBuilderMonad m) =>
  -- | Addr of the funds
  GYAddress ->
  -- | Wallet ref
  GYTxOutRef ->
  -- | Amount of funds
  Integer ->
  -- | Collateral amount
  Integer ->
  -- | Signer
  GYPubKeyHash ->
  m (GYTxSkeleton 'PlutusV2)
mkCreateCollateralSkeleton addr walletRef fundsAmt collateralAmt signer = do
  pure $
    -- wallet input
    mustHaveInput
      ( GYTxIn
          { gyTxInTxOutRef = walletRef,
            gyTxInWitness = GYTxInWitnessKey
          }
      )
      <> mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = addr,
              gyTxOutValue = valueSingleton GYLovelace fundsAmt,
              gyTxOutDatum = Nothing,
              gyTxOutRefS = Nothing
            }
        )
      -- collateral
      <> mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = addr,
              gyTxOutValue = valueSingleton GYLovelace collateralAmt,
              gyTxOutDatum = Nothing,
              gyTxOutRefS = Nothing
            }
        )
      <> mustBeSignedBy signer
