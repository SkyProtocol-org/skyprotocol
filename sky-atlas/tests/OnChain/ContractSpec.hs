{-# LANGUAGE OverloadedStrings #-}

module OnChain.ContractSpec (contractSpec) where

import API.Bounty.Contracts
import API.Bridge.Contracts
import Common.Crypto (computeDigest)
import Common.Types (Byte (..))
import Control.Monad.Extra (maybeM)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
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
        "Compiling contracts"
        [ mkTestFor "Create Minting Policy" mintingPolicyTest,
          mkTestFor "Send funds" sendFundsTest
          -- mkTestFor "Update bridge" updateBridgeTest
        ]
    ]

-- updateBridgeTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
-- updateBridgeTest TestInfo {..} = do
--   addr <- getUserAddr $ admin testWallets
--   gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
--   pkh <- addressToPubKeyHash' addr
--   let datum = BridgeNFTDatum $ computeDigest (Byte 1)

--   asUser (admin testWallets) $ do
--     updateBridgeSkeleton <- mkUpdateBridgeSkeleton bridgeValidator bridgeUtxo datum redeemer "TestSkyToken" addr pkh
--     gyLogDebug' "" $ printf "tx skeleton: %s" (show updateBridgeSkeleton)
--     txId <- buildTxBody updateBridgeSkeleton >>= signAndSubmitConfirmed
--     gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

sendFundsTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
sendFundsTest TestInfo {..} = do
  addr <- getUserAddr $ admin testWallets
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
  pkh <- addressToPubKeyHash' addr
  let amount = 10

  -- TODO: make a transaction with bounty contract and then try to send some funds there
  -- cauze rn it check only the fact, that the skeleton is successfully built and "executed"
  -- with no way to actually check the balances
  asUser (admin testWallets) $ do
    sendFundsSkeleton <- mkSendSkeleton addr amount GYLovelace pkh
    gyLogDebug' "" $ printf "tx skeleton: %s" (show sendFundsSkeleton)
    txId <- buildTxBody sendFundsSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

mintingPolicyTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
mintingPolicyTest TestInfo {..} = do
  addr <- getAddr
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
  pkh <- addressToPubKeyHash' addr

  asUser (admin testWallets) $ do
    mintSkeleton <- mkMintingSkeleton "TestSkyToken" pkh (computeDigest (Byte 1))
    gyLogDebug' "" $ printf "tx skeleton: %s" (show mintSkeleton)
    txId <- buildTxBody mintSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

getAddr :: (GYTxUserQueryMonad m) => m GYAddress
getAddr =
  maybeM
    (throwAppError $ someBackendError "No own addresses")
    pure
    $ listToMaybe
      <$> ownAddresses

getUserAddr :: (GYTxUserQueryMonad m) => User -> m GYAddress
getUserAddr user = pure . NE.head $ userAddresses user
