{-# LANGUAGE OverloadedStrings #-}

module OnChain.BountySpec (bountySpec) where

import API.Bounty.Contracts
import API.SkyMintingPolicy
import Common
import Contract.Bounty (ClientParams (..), ClientRedeemer (..), validateClaimBounty, validateTimeout)
import Contract.Bridge (BridgeParams (..))
import Contract.DaH
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import OnChain.BridgeSpec (updateBridgeTest)
import OnChain.MintingPolicySpec (mintingPolicyTest)
import PlutusLedgerApi.V1.Time qualified as T
import PlutusLedgerApi.V2
import PlutusTx.Prelude (BuiltinString)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Utils

-- Test constants
testDeadline :: POSIXTime
testDeadline = 1000180800000 -- Sep 11th 2001

-- Time intervals
beforeDeadline :: Interval POSIXTime
beforeDeadline =
  Interval
    (strictLowerBound 999316800000) -- Sep 1st 2001
    (strictUpperBound 1000008000000) -- Sep 9th 2001

afterDeadline :: Interval POSIXTime
afterDeadline =
  Interval
    (strictLowerBound 1000267200000) -- Sep 12th 2001
    (strictUpperBound 1000872000000) -- Sep 19th 2001

aroundDeadline :: Interval POSIXTime
aroundDeadline =
  Interval
    (strictLowerBound 1000008000000) -- Sep 9th 2001
    (strictUpperBound 1000872000000) -- Sep 19th 2001

-- Test data setup
testMessage :: BuiltinByteString
testMessage = "Hello, Bounty World!"

testMessageHash :: Hash
testMessageHash = computeDigest testMessage

-- Create test DA structure and proof
createTestData :: (TopicId, MessageId, SkyDataProofH, Hash)
createTestData =
  let daSchema = computeDigest @Hash @Bytes4 $ ofHex "deadbeef"
      sk1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
      sk2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"
      pk1 = derivePubKey sk1
      pk2 = derivePubKey sk2
      committee = MultiSigPubKey ([pk1, pk2], UInt16 2)
      topicSchema = computeDigest @Hash @Bytes4 $ ofHex "1ea7f00d"
      timestamp = POSIXTime 455155200000 -- June 4th 1989
      da0 = runIdentity $ initDa daSchema committee :: SkyDa (HashRef Hash)
      (da1, maybeTopicId) = runIdentity $ insertTopic topicSchema da0
      topicId = fromJust maybeTopicId
      (da2, maybeMessageId) = runIdentity $ insertMessage timestamp testMessage topicId da1
      messageId = fromJust maybeMessageId
      (_messageRef, proof) = fromJust . runIdentity $ getSkyDataProofH (topicId, messageId) da2 :: (Hash, SkyDataProofH)
      topHash = computeDigest da2
   in (topicId, messageId, proof, topHash)

-- Extract test components
testTopicIdFromDa :: TopicId
testMessageId :: MessageId
testProof :: SkyDataProofH
testTopHash :: Hash
(testTopicIdFromDa, testMessageId, testProof, testTopHash) = createTestData

-- Create invalid proofs for negative testing
createInvalidProof :: SkyDataProofH
createInvalidProof =
  let daSchema = computeDigest @Hash @Bytes4 $ ofHex "baadf00d"
      sk1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
      pk1 = derivePubKey sk1
      committee = MultiSigPubKey ([pk1], UInt16 1)
      topicSchema = computeDigest @Hash @Bytes4 $ ofHex "deadc0de"
      timestamp = POSIXTime 455155200000
      wrongMessage = "Wrong message content"
      da0 = runIdentity $ initDa daSchema committee :: SkyDa (HashRef Hash)
      (da1, maybeTopicId) = runIdentity $ insertTopic topicSchema da0
      topicId = fromJust maybeTopicId
      (da2, maybeMessageId) = runIdentity $ insertMessage timestamp wrongMessage topicId da1
      messageId = fromJust maybeMessageId
      (_messageRef, proof) = fromJust . runIdentity $ getSkyDataProofH (topicId, messageId) da2 :: (Hash, SkyDataProofH)
   in proof

invalidProof :: SkyDataProofH
invalidProof = createInvalidProof

bountySpec :: TestTree
bountySpec =
  testGroup
    "Bounty Contract"
    [ testGroup
        "validateClaimBounty"
        [ testCase "should validate correct bounty claim within deadline" $ do
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              testTopicIdFromDa
              testProof
              testTopHash
              @?= True,
          testCase "should reject bounty claim after deadline" $ do
            validateClaimBounty
              testDeadline
              afterDeadline
              testMessageHash
              testTopicIdFromDa
              testProof
              testTopHash
              @?= False,
          testCase "should reject bounty claim with wrong message hash" $ do
            let wrongHash = computeDigest @Hash ("wrong message" :: BuiltinString)
            validateClaimBounty
              testDeadline
              beforeDeadline
              wrongHash
              testTopicIdFromDa
              testProof
              testTopHash
              @?= False,
          testCase "should reject bounty claim with wrong topic ID" $ do
            let wrongTopicId = topicIdFromInteger 999
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              wrongTopicId
              testProof
              testTopHash
              @?= False,
          testCase "should reject bounty claim with wrong DA top hash" $ do
            let wrongTopHash = computeDigest @Hash ("wrong top hash" :: BuiltinString)
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              testTopicIdFromDa
              testProof
              wrongTopHash
              @?= False,
          testCase "should reject bounty claim with invalid proof" $ do
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              testTopicIdFromDa
              invalidProof
              testTopHash
              @?= False,
          testCase "should validate proof structure requirements" $ do
            -- Check that proof has correct height and key structure
            triePathHeight (proofTopicTriePathH testProof) @?= 0
            triePathKey (proofTopicTriePathH testProof) @?= testTopicIdFromDa
            triePathHeight (proofMessageTriePathH testProof) @?= 0
            triePathKey (proofMessageTriePathH testProof) @?= testMessageId
        ],
      testGroup
        "validateTimeout"
        [ testCase "should validate timeout after deadline" $ do
            validateTimeout testDeadline afterDeadline @?= True,
          testCase "should reject timeout before deadline" $ do
            validateTimeout testDeadline beforeDeadline @?= False,
          testCase "should reject timeout when interval spans deadline" $ do
            validateTimeout testDeadline aroundDeadline @?= False,
          testCase "should validate timeout exactly at deadline boundary" $ do
            let exactlyAfter =
                  Interval
                    (strictLowerBound testDeadline)
                    (strictUpperBound (testDeadline + 1000))
            validateTimeout testDeadline exactlyAfter @?= True,
          testCase "should reject timeout exactly before deadline boundary" $ do
            let exactlyBefore =
                  Interval
                    (strictLowerBound (testDeadline - 1000))
                    (strictUpperBound testDeadline)
            validateTimeout testDeadline exactlyBefore @?= False
        ],
      testGroup
        "Proof validation integration"
        [ testCase "should validate applySkyDataProof correctly" $ do
            -- Verify that the proof actually validates the message hash
            let proofResult = applySkyDataProofH testProof testMessageHash
            proofResult @?= testTopHash,
          testCase "should reject proof with wrong message hash" $ do
            let wrongHash = computeDigest @Hash ("different message" :: BuiltinString)
                proofResult = applySkyDataProofH testProof wrongHash
            proofResult == testTopHash @?= False,
          testCase "should validate proof structure matches topic requirements" $ do
            -- The proof should have the correct topic ID in its path
            let topicPath = proofTopicTriePathH testProof
            triePathKey topicPath @?= testTopicIdFromDa
            -- Height should be 0 (direct path from leaf to root)
            triePathHeight topicPath @?= 0
        ],
      testGroup
        "Edge cases and error conditions"
        [ testCase "should handle zero deadline correctly" $ do
            let zeroDeadline = POSIXTime 0
                afterZero = Interval (strictLowerBound 1) (strictUpperBound 1000)
                beforeZero = Interval (strictLowerBound (-1000)) (strictUpperBound (-1))
            validateTimeout zeroDeadline afterZero @?= True
            validateTimeout zeroDeadline beforeZero @?= False,
          testCase "should handle maximum POSIXTime values" $ do
            let maxTime = POSIXTime 9223372036854775807 -- Max Int64
                beforeMax = Interval (strictLowerBound 0) (strictUpperBound 1000000000000)
                afterMax = Interval (strictLowerBound maxTime) (strictUpperBound maxTime)
            validateTimeout maxTime beforeMax @?= False
            validateTimeout maxTime afterMax @?= True,
          testCase "should validate empty topic ID correctly" $ do
            let emptyTopicId = topicIdFromInteger 0
            -- This should work with our test proof since we created it with topic 0
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              emptyTopicId
              testProof
              testTopHash
              @?= True
        ],
      testGroup
        "Running contracts"
        [ mkTestFor "Claim bounty" $ \TestInfo {..} -> do
            let uvk = userPaymentVKey $ admin testWallets
                -- da0 initial version, to initialize the bridge with
                initialState@(initialDa, _schema, _committee) = createTestDa uvk
                topH0 = computeDigest initialDa

            let -- updatedDa updated version (da1 intermediate one), to update the bridge to
                (da1, maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) initialDa
                topicId = fromJust maybeTopicId
                (updatedDa, maybeMessageId) = runIdentity $ insertMessage (POSIXTime 32132) testMessage topicId da1
                messageHash = computeDigest testMessage
                (_messageRef, proof) = fromJust . runIdentity $ getSkyDataProofH (topicId, fromJust maybeMessageId) updatedDa :: (Hash, SkyDataProofH)

            let deadline = unsafePerformIO currentPOSIXTime

            mintingPolicyTest TestInfo {..} topH0
            updateBridgeTest TestInfo {..} initialState updatedDa
            sendFundsTest TestInfo {..} topicId messageHash deadline
            claimBountyTest TestInfo {..} topicId messageHash deadline proof
        ]
    ]

sendFundsTest ::
  ( GYTxGameMonad m,
    GYTxUserQueryMonad m
  ) =>
  TestInfo ->
  TopicId ->
  Hash ->
  T.POSIXTime ->
  m ()
sendFundsTest TestInfo {..} topicId messageHash deadline = do
  adminAddr <- getUserAddr $ admin testWallets
  offererAddr <- getUserAddr $ offerer testWallets
  claimantAddr <- getUserAddr $ claimant testWallets

  adminPkh <- addressToPubKeyHash' adminAddr
  offererPkh <- addressToPubKeyHash' offererAddr
  claimantPkh <- addressToPubKeyHash' claimantAddr

  let skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus adminPkh
      bountyNFTCurrencySymbol = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
      bountyClaimantPubKeyHash = pubKeyHashToPlutus claimantPkh
      bountyOffererPubKeyHash = pubKeyHashToPlutus offererPkh
      clientParams =
        ClientParams
          { bountyTopicId = topicId,
            bountyMessageHash = messageHash,
            bountyDeadline = deadline,
            ..
          }

  bountyVAddr <- bountyValidatorAddress clientParams

  asUser (offerer testWallets) $ do
    sendFundsSkeleton <- mkSendSkeleton bountyVAddr 10_000_000 GYLovelace offererPkh
    -- gyLogDebug' "" $ printf "tx skeleton: %s" (show sendFundsSkeleton)
    txId <- buildTxBody sendFundsSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

claimBountyTest ::
  ( GYTxGameMonad m,
    GYTxUserQueryMonad m
  ) =>
  TestInfo ->
  TopicId ->
  Hash ->
  T.POSIXTime ->
  SkyDataProofH ->
  m ()
claimBountyTest TestInfo {..} topicId messageHash deadline proof = do
  adminAddr <- getUserAddr $ admin testWallets
  offererAddr <- getUserAddr $ offerer testWallets
  claimantAddr <- getUserAddr $ claimant testWallets

  adminPkh <- addressToPubKeyHash' adminAddr
  offererPkh <- addressToPubKeyHash' offererAddr
  claimantPkh <- addressToPubKeyHash' claimantAddr

  let skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus adminPkh
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId "SkyBridge"
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy
      bountyNFTCurrencySymbol = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
      bountyClaimantPubKeyHash = pubKeyHashToPlutus claimantPkh
      bountyOffererPubKeyHash = pubKeyHashToPlutus offererPkh
      clientParams =
        ClientParams
          { bountyTopicId = topicId,
            bountyMessageHash = messageHash,
            bountyDeadline = deadline,
            ..
          }

  bridgeUtxos <- do
    addr <- bridgeValidatorAddress $ BridgeParams curSym
    utxosAtAddressWithDatums addr $ Just skyToken
  bountyUtxos <- do
    addr <- bountyValidatorAddress clientParams
    utxosAtAddress addr Nothing

  let utxoWithDatum = flip filter bridgeUtxos $ \(out, _) ->
        let assets = valueToList $ utxoValue out
         in flip any assets $ \case
              (GYToken pId name, _) -> name == "SkyBridge" && pId == skyPolicyId
              _ -> False
  nftRef <- case utxoWithDatum of
    [(utxo, Just _)] -> pure $ utxoRef utxo
    _ -> throwAppError $ someBackendError "Can't find bridge utxo"

  -- TODO: make this safe
  let bountyAmount =
        snd
          . head -- get first asset. TODO: search for the one we need
          . valueToList
          . utxoValue
          . head -- get the first utxo. TODO: search for the one we need
          . utxosToList
          $ bountyUtxos
      redeemer = ClaimBounty $ toByteString proof

  asUser (offerer testWallets) $ do
    sendFundsSkeleton <-
      mkClaimBountySkeleton
        (bountyValidator' clientParams)
        nftRef
        redeemer
        claimantAddr
        bountyAmount
        offererPkh
    -- gyLogDebug' "" $ printf "tx skeleton: %s" (show sendFundsSkeleton)
    txId <- buildTxBody sendFundsSkeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId
