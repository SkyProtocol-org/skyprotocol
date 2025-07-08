{-# LANGUAGE OverloadedStrings #-}

module OnChain.ContractSpec (contractSpec) where

import API.SkyMintingPolicy
import Contract.SkyBridge
import Control.Monad.Extra (maybeM)
import Data.Maybe (listToMaybe)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Test.Tasty

-- | Test environment 'WalletInfo' among other things provides nine wallets that
-- be used in tests. For convinience we assign some meaningful names to them.
admin, oracle, holder :: Wallets -> User
admin = w1 -- Runs some administrative action, e.g. deplys the script
oracle = w8 -- A user that is going to reveal the answer
holder = w9 -- A user to store the reference script

-- TODO: setup a privnet 3 node testing for this from atlas
-- https://atlas-app.io/getting-started/testing
contractSpec :: TestTree
contractSpec =
  testGroup
    "OnChain Contract Tests"
    [ testGroup
        "Sky Minting Policy"
        [ mkTestFor "simple transaction test" simpleTxTest,
          mkTestFor "minting policy" mintingPolicyTest
        ]
    ]

mintingPolicyTest :: (GYTxGameMonad m) => TestInfo -> m ()
mintingPolicyTest TestInfo {..} = do
  asUser (admin testWallets) $ do
    mintSkeleton <- mkMintingSkeleton
    gyLogDebug' "" $ printf "tx skeleton: %s" (show mintSkeleton)
    txId <- buildTxBody mintSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

mkMintingSkeleton :: (GYTxUserQueryMonad m) => m (GYTxSkeleton 'PlutusV2)
mkMintingSkeleton = do
  addr <-
    maybeM
      (throwAppError $ someBackendError "No own addresses")
      pure
      $ listToMaybe
        <$> ownAddresses
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)

  pkh <- addressToPubKeyHash' addr

  let bridgeDatum = BridgeNFTDatum ""
      tokenName = "SkyBridgeToken"
      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus pkh
      skyToken = GYToken (mintingPolicyId skyPolicy) tokenName
      amt = 1
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bvAddr <- bridgeValidatorAddress $ BridgeParams curSym

  -- skeleton for minting transaction
  pure $
    mustMint @'PlutusV2 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName amt
      <> mustHaveOutput
        ( GYTxOut
            { -- recipient is the bridge validator
              gyTxOutAddress = bvAddr,
              gyTxOutValue = valueSingleton skyToken amt,
              gyTxOutDatum = Just (datumFromPlutusData bridgeDatum, GYTxOutUseInlineDatum),
              gyTxOutRefS = Nothing
            }
        )
      <> mustBeSignedBy pkh

-- | Trace for a super-simple spending transaction. This function combines
-- the  runner and the test for simplicity's sake.
simpleTxTest :: (GYTxGameMonad m) => TestInfo -> m ()
simpleTxTest (testWallets -> Wallets {w1}) = do
  withWalletBalancesCheckSimple [w1 := valueFromLovelace (-100_000_000)]
    . asUser w1
    $ do
      skeleton <- mkTrivialTx
      gyLogDebug' "" $ printf "tx skeleton: %s" (show skeleton)
      txId <- buildTxBody skeleton >>= signAndSubmitConfirmed
      gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

-- Pretend off-chain code written in 'GYTxUserQueryMonad m'
mkTrivialTx :: (GYTxUserQueryMonad m) => m (GYTxSkeleton 'PlutusV2)
mkTrivialTx = do
  addr <-
    maybeM
      (throwAppError $ someBackendError "No own addresses")
      pure
      $ listToMaybe
        <$> ownAddresses
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
  pkh <- addressToPubKeyHash' addr
  let targetAddr = unsafeAddressFromText "addr_test1qr2vfntpz92f9pawk8gs0fdmhtfe32pqcx0s8fuztxaw3p5pjay24kygaj4g8uevf89ewxzvsdc60wln8spzm2al059q8a9w3x"
  return $
    mustHaveOutput
      ( GYTxOut
          { gyTxOutAddress = targetAddr,
            gyTxOutValue = valueFromLovelace 100_000_000,
            gyTxOutDatum = Nothing,
            gyTxOutRefS = Nothing
          }
      )
      <> mustBeSignedBy pkh
