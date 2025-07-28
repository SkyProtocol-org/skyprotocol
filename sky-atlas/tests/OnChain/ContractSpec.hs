{-# LANGUAGE OverloadedStrings #-}

module OnChain.ContractSpec (contractSpec) where

import API.Bounty.Contracts
import API.Bridge.Contracts
import API.SkyMintingPolicy
import Common
import Contract.Bridge
import Control.Monad.Extra (maybeM)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty
import Util

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
          mkTestFor "Send funds" sendFundsTest,
          mkTestFor "Update bridge" updateBridgeTest
        ]
    ]

updateBridgeTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
updateBridgeTest TestInfo {..} = do
  addr <- getUserAddr $ admin testWallets
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
  pkh <- addressToPubKeyHash' addr

  let uvk = userPaymentVKey $ admin testWallets

  let (da, schema, committee) = createTestDa uvk

  let (da', maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da
      topicId = fromJust maybeTopicId
      (newDa, _maybeMessageId) = runIdentity $ insertMessage (POSIXTime 32132) "test message" topicId da'
      oldTopHash = computeDigest da
      topHash = computeDigest @Blake2b_256 newDa

  let bridgeSchema = schema
      bridgeCommittee = committee
      bridgeOldRootHash = oldTopHash
      bridgeNewTopHash = topHash
      -- TODO: make this safe
      adminSecKeyBytes = signingKeyToRawBytes $ userPaymentSKey' $ admin testWallets
      adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
      signature = signMessage adminSecKey topHash
      adminPubKeyBytes = paymentVerificationKeyRawBytes uvk
      adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
      bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
      bridgeRedeemer = UpdateBridge {..}

  let skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus pkh
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId "536B79427269646765"
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  -- create bridge and mint some nft
  asUser (admin testWallets) $ do
    mintSkeleton <- mkMintingSkeleton "536B79427269646765" pkh oldTopHash
    gyLogDebug' "" $ printf "tx skeleton: %s" (show mintSkeleton)
    txId <- buildTxBody mintSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

  bridgeUtxos <- do
    bridgeVAddr <- bridgeValidatorAddress $ BridgeParams curSym
    utxosAtAddress bridgeVAddr $ Just skyToken

  gyLogDebug' "" $ printf "bridge utxos: %s" bridgeUtxos

  let utxo = flip filter (utxosToList bridgeUtxos) $ \out ->
        let assets = valueToList $ utxoValue out
         in flip any assets $ \case
              (GYToken pId name, _) -> pId == skyPolicyId
              _ -> False
  bridgeUtxo <- case utxo of
    u : _ -> pure u
    _ -> throwAppError $ someBackendError "Can't find bridge utxo"

  -- update bridge
  asUser (admin testWallets) $ do
    updateBridgeSkeleton <-
      mkUpdateBridgeSkeleton
        (bridgeValidator' $ BridgeParams curSym)
        (utxoRef bridgeUtxo)
        (BridgeNFTDatum topHash)
        bridgeRedeemer
        skyToken
        addr
        pkh
    gyLogDebug' "" $ printf "tx skeleton: %s" (show updateBridgeSkeleton)
    txId <- buildTxBody updateBridgeSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

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
