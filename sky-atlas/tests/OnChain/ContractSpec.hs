{-# LANGUAGE OverloadedStrings #-}

module OnChain.ContractSpec (contractSpec) where

import API.Bridge.Contracts
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
-- import PlutusLedgerApi.V2.Contexts (ScriptContext (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusLedgerApi.V2
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
        [ mkTestFor "Create Minting Policy" mintingPolicyTest
        ]
    ]

bridgeValidatorTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
bridgeValidatorTest = do
  undefined

mintingPolicyTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
mintingPolicyTest TestInfo {..} = do
  addr <- getAddr
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
  pkh <- addressToPubKeyHash' addr

  asUser (admin testWallets) $ do
    mintSkeleton <- mkMintingSkeleton Nothing "TestSkyToken" pkh ""
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
