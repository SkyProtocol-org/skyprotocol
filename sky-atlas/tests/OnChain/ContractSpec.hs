{-# LANGUAGE OverloadedStrings #-}

module OnChain.ContractSpec (contractSpec) where

import API.Bounty.Contracts
import API.Bridge.Contracts
import API.SkyMintingPolicy
import Common
import Contract.Bridge
import Control.Monad.Extra (maybeM)
import Control.Monad.IO.Class
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
import PlutusTx.Builtins.Internal (BuiltinByteString (..), BuiltinString)
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

      -- da0 initial version, to initialize the bridge with
      (da0, schema, committee) = createTestDa uvk
      daMetaH0 = refDigest (skyMetaData da0)
      daTopicsH0 = refDigest (skyTopicTrie da0)
      topH0 = computeDigest da0

      -- da2 updated version (da1 intermediate one), to update the bridge to
      (da1, maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da0
      topicId = fromJust maybeTopicId
      (da2, _maybeMessageId) = runIdentity $ insertMessage (POSIXTime 32132) "test message" topicId da1
      topH2 = computeDigest @Hash da2
      daMetaH2 = refDigest (skyMetaData da2) -- should be same as daMetaH0
      daTopicsH2 = refDigest (skyTopicTrie da2)

      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus pkh
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId "SkyBridge"
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  gyLogDebug' "" $ printf "AAAA daMetaH0: %s" (hexOf daMetaH0 :: String)
  gyLogDebug' "" $ printf "AAAA daTopicsH0: %s" (hexOf daTopicsH0 :: String)
  gyLogDebug' "" $ printf "AAAA topH0: %s" (hexOf topH0 :: String)
  gyLogDebug' "" $ printf "AAAA recomputed topH0: %s" (hexOf . computeDigest @Hash $ (daMetaH0, daTopicsH0) :: String)

  gyLogDebug' "" $ printf "AAAA daMetaH2: %s" (hexOf daMetaH2 :: String)
  gyLogDebug' "" $ printf "AAAA daTopicsH2: %s" (hexOf daTopicsH2 :: String)
  gyLogDebug' "" $ printf "AAAA topH2: %s" (hexOf topH2 :: String)

  bridgeVAddr <- bridgeValidatorAddress $ BridgeParams curSym
  -- create bridge and mint some nft
  asUser (admin testWallets) $ do
    mintSkeleton <-
      mkMintingSkeleton
        "SkyBridge"
        skyToken
        skyPolicy
        topH0
        bridgeVAddr
        pkh
    gyLogDebug' "" $ printf "tx skeleton: %s" (show mintSkeleton)
    txId <- buildTxBody mintSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

  bridgeUtxos <- utxosAtAddress bridgeVAddr $ Just skyToken

  gyLogDebug' "" $ printf "bridge utxos: %s" bridgeUtxos

  let utxo = flip filter (utxosToList bridgeUtxos) $ \out ->
        let assets = valueToList $ utxoValue out
         in flip any assets $ \case
              (GYToken pId _name, _) -> pId == skyPolicyId
              _ -> False
  bridgeUtxo <- case utxo of
    [u] -> do
      gyLogDebug' "" $ printf "bridge utxo: %s" (show u)
      pure u
    _ -> throwAppError $ someBackendError "Can't find bridge utxo"

  let bridgeSchema = schema
      bridgeCommittee = committee
      bridgeOldRootHash = daTopicsH0
      bridgeNewTopHash = topH2
      -- TODO: make this safe
      adminSecKeyBytes = signingKeyToRawBytes $ userPaymentSKey' $ admin testWallets
      adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
      signature = signMessage adminSecKey topH2
      adminPubKeyBytes = paymentVerificationKeyRawBytes uvk
      adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
      bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
      bridgeRedeemer = UpdateBridge {..}

  -- update bridge
  asUser (admin testWallets) $ do
    updateBridgeSkeleton <-
      mkUpdateBridgeSkeleton
        (bridgeValidator' $ BridgeParams curSym)
        (utxoRef bridgeUtxo)
        (BridgeNFTDatum topH2)
        bridgeRedeemer
        skyToken
        bridgeVAddr
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
    -- gyLogDebug' "" $ printf "tx skeleton: %s" (show sendFundsSkeleton)
    txId <- buildTxBody sendFundsSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

mintingPolicyTest :: (GYTxGameMonad m, GYTxUserQueryMonad m) => TestInfo -> m ()
mintingPolicyTest TestInfo {..} = do
  addr <- getAddr
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
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
        (computeDigest (Byte 1)) -- dummy top hash
        bridgeVAddr
        pkh
    -- gyLogDebug' "" $ printf "tx skeleton: %s" (show mintSkeleton)
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
