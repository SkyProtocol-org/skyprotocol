{-# LANGUAGE OverloadedStrings #-}

module OnChain.MintingPolicySpec (mintingPolicySpec, mintingPolicyTest) where

import API.Bridge.Contracts (mkMintingSkeleton)
import API.SkyMintingPolicy
import Common
import Contract.Bridge
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Test.Tasty
import Util

mintingPolicySpec :: TestTree
mintingPolicySpec =
  testGroup
    "CLB emulator test for minting policy"
    [ testGroup
        "Running contract"
        [ mkTestFor "Create Minting Policy" $ \testInfo -> do
            let topHash = computeDigest (Byte 1) -- dummy top hash
            mintingPolicyTest testInfo topHash
        ]
    ]

mintingPolicyTest ::
  ( GYTxGameMonad m,
    GYTxUserQueryMonad m
  ) =>
  TestInfo ->
  Hash ->
  m ()
mintingPolicyTest TestInfo {..} topHash = do
  addr <- getUserAddr $ admin testWallets
  pkh <- addressToPubKeyHash' addr

  let skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus pkh
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId "SkyBridge"
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bridgeVAddr <- bridgeValidatorAddress $ BridgeParams curSym

  asUser (admin testWallets) $ do
    mintSkeleton <-
      mkMintingSkeleton
        "SkyBridge"
        skyToken
        skyPolicy
        topHash
        bridgeVAddr
        pkh
    -- gyLogDebug' "" $ printf "tx skeleton: %s" (show mintSkeleton)
    txId <- buildTxBody mintSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId
