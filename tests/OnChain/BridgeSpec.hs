{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.BridgeSpec (bridgeSpec, updateBridgeTest) where

import API.SkyMintingPolicy
import App.Env hiding (getCardanoUser)
import Common
import Contract.Bridge
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import OnChain.MintingPolicySpec (mintingPolicyTest)
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty
import Test.Tasty.HUnit
import Transaction.Bridge (mkUpdateBridgeSkeleton)
import Util
import Utils
import Prelude

bridgeSpec :: TestTree
bridgeSpec =
  testGroup
    "Bridge Contract"
    [ testGroup
        "Bridge validator"
        [ testCase "to/from builtin data BridgeNFTDatum" $ do
            let datum = BridgeNFTDatum $ ofHex "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
            let builtinDatum = PlutusTx.toBuiltinData datum
            PlutusTx.fromBuiltinData builtinDatum @?= Just datum,
          testCase "multiSigValid" $ do
            adminCU <- getCardanoUser
            let (da', _schema, committee) = createTestDa $ cuserVerificationKey adminCU
            timestamp <- currentPOSIXTime

            let adminSecKeyBytes = let (AGYPaymentSigningKey sk) = cuserSigningKey adminCU in signingKeyToRawBytes sk
                adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
                adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey adminCU
                adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
                (da'', maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da'
                topicId = fromJust maybeTopicId
                (da, _maybeMessageId) = runIdentity $ insertMessage timestamp "test message" topicId da''
                topHash = computeDigest @Blake2b_256 da
                signature = signMessage adminSecKey topHash
                bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
            multiSigValid committee topHash bridgeSig @?= True,
          testCase "bridgeTypedValidatorCore" $ do
            adminCU <- getCardanoUser
            let (da', schema, committee) = createTestDa $ cuserVerificationKey adminCU
            timestamp <- currentPOSIXTime

            let adminSecKeyBytes = let (AGYPaymentSigningKey sk) = cuserSigningKey adminCU in signingKeyToRawBytes sk
                adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
                adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey adminCU
                adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
                oldTopHash = computeDigest @Blake2b_256 da'
                (da'', maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da'
                topicId = fromJust maybeTopicId
                (da, _maybeMessageId) = runIdentity $ insertMessage timestamp "test message" topicId da''
                daDataHash' = refDigest . skyTopicTrie $ da'

            daData <- unwrap $ skyTopicTrie da'

            let topHash = computeDigest @Blake2b_256 da
                signature = signMessage adminSecKey topHash
                bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
                daDataHash = computeDigest @Blake2b_256 daData

            daDataHash' @?= daDataHash
            bridgeTypedValidatorCore schema committee daDataHash topHash bridgeSig oldTopHash @?= True
        ],
      testGroup
        "Running contracts"
        [ mkTestFor "Update bridge" $ \TestInfo {..} -> do
            let uvk = userPaymentVKey $ admin testWallets
                -- da0 initial version, to initialize the bridge with
                initialState@(initialDa, _schema, _committee) = createTestDa uvk
                topH0 = computeDigest initialDa

            let -- updatedDa updated version (da1 intermediate one), to update the bridge to
                (da1, maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) initialDa
                topicId = fromJust maybeTopicId
                (updatedDa, _maybeMessageId) = runIdentity $ insertMessage (POSIXTime 32132) "test message" topicId da1

            mintingPolicyTest TestInfo {..} topH0
            updateBridgeTest TestInfo {..} initialState updatedDa
        ]
    ]

updateBridgeTest ::
  ( GYTxGameMonad m,
    GYTxUserQueryMonad m
  ) =>
  TestInfo ->
  (SkyDa (HashRef Hash), Hash, MultiSigPubKey) ->
  SkyDa (HashRef Hash) ->
  m ()
updateBridgeTest TestInfo {..} (initialDa, schema, committee) updatedDa = do
  addr <- getUserAddr $ admin testWallets
  pkh <- addressToPubKeyHash' addr

  let topH2 = computeDigest @Hash updatedDa

      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus pkh
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId "SkyBridge"
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bridgeVAddr <- bridgeValidatorAddress $ BridgeParams curSym
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
      bridgeOldRootHash = refDigest (skyTopicTrie initialDa)
      bridgeNewTopHash = topH2
      -- TODO: make this safe
      adminSecKeyBytes = signingKeyToRawBytes $ userPaymentSKey' $ admin testWallets
      adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
      signature = signMessage adminSecKey topH2
      uvk = userPaymentVKey $ admin testWallets
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
    -- gyLogDebug' "" $ printf "tx skeleton: %s" (show updateBridgeSkeleton)
    txId <- buildTxBody updateBridgeSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId
