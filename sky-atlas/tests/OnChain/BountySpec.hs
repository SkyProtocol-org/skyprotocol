{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.BountySpec (bountySpec) where

import Common
import Contract.Bounty (ClientRedeemer (..), validateClaimBounty, validateTimeout)
import Data.Functor.Identity (Identity (..))
import PlutusLedgerApi.V2
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit

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
createTestDaAndProof :: (SkyDa (HashRef Hash), TopicId, MessageId, SkyDataProof Hash, Hash)
createTestDaAndProof =
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
      (_messageRef, proof) = fromJust . runIdentity $ getSkyDataProof (topicId, messageId) da2 :: (LiftRef (HashRef Hash) BBS, SkyDataProof Hash)
      topHash = computeDigest da2
   in (da2, topicId, messageId, proof, topHash)

-- Extract test components
_testDa :: SkyDa (HashRef Hash)
testTopicIdFromDa :: TopicId
testMessageId :: MessageId
testProof :: SkyDataProof Blake2b_256
testTopHash :: Hash
(_testDa, testTopicIdFromDa, testMessageId, testProof, testTopHash) = createTestDaAndProof

-- Type annotations for clarity
testProofTyped :: SkyDataProof Blake2b_256
testProofTyped = testProof

-- Create invalid proofs for negative testing
createInvalidProof :: SkyDataProof Blake2b_256
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
      (_messageRef, proof) = fromJust . runIdentity $ getSkyDataProof (topicId, messageId) da2 :: (LiftRef (HashRef Hash) BBS, SkyDataProof Hash)
   in proof

invalidProof :: SkyDataProof Blake2b_256
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
              testProofTyped
              testTopHash
              @?= True,
          testCase "should reject bounty claim after deadline" $ do
            validateClaimBounty
              testDeadline
              afterDeadline
              testMessageHash
              testTopicIdFromDa
              testProofTyped
              testTopHash
              @?= False,
          testCase "should reject bounty claim with wrong message hash" $ do
            let wrongHash = computeDigest @Hash ("wrong message" :: BuiltinString)
            validateClaimBounty
              testDeadline
              beforeDeadline
              wrongHash
              testTopicIdFromDa
              testProofTyped
              testTopHash
              @?= False,
          testCase "should reject bounty claim with wrong topic ID" $ do
            let wrongTopicId = topicIdFromInteger 999
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              wrongTopicId
              testProofTyped
              testTopHash
              @?= False,
          testCase "should reject bounty claim with wrong DA top hash" $ do
            let wrongTopHash = computeDigest @Hash ("wrong top hash" :: BuiltinString)
            validateClaimBounty
              testDeadline
              beforeDeadline
              testMessageHash
              testTopicIdFromDa
              testProofTyped
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
            triePathHeight (pathTopicTriePath testProofTyped) @?= 0
            triePathKey (pathTopicTriePath testProofTyped) @?= testTopicIdFromDa
            triePathHeight (pathMessageTriePath testProofTyped) @?= 0
            triePathKey (pathMessageTriePath testProofTyped) @?= testMessageId
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
        "ClientRedeemer structure"
        [ testCase "should construct ClaimBounty redeemer correctly" $ do
            let redeemer = ClaimBounty testProofTyped
            case redeemer of
              ClaimBounty _ -> True @?= True
              _ -> assertFailure "Expected ClaimBounty constructor",
          testCase "should construct Timeout redeemer correctly" $ do
            let redeemer = Contract.Bounty.Timeout
            case redeemer of
              Contract.Bounty.Timeout -> True @?= True
              _ -> assertFailure "Expected Timeout constructor",
          testCase "should distinguish between redeemer types" $ do
            let claimRedeemer = ClaimBounty testProofTyped
                timeoutRedeemer = Contract.Bounty.Timeout
            case (claimRedeemer, timeoutRedeemer) of
              (ClaimBounty _, Contract.Bounty.Timeout) -> True @?= True
              _ -> assertFailure "Redeemer types should be different"
        ],
      testGroup
        "Proof validation integration"
        [ testCase "should validate applySkyDataProof correctly" $ do
            -- Verify that the proof actually validates the message hash
            let proofResult = applySkyDataProof testProofTyped testMessageHash
            proofResult @?= testTopHash,
          testCase "should reject proof with wrong message hash" $ do
            let wrongHash = computeDigest @Hash ("different message" :: BuiltinString)
                proofResult = applySkyDataProof testProofTyped wrongHash
            proofResult == testTopHash @?= False,
          testCase "should validate proof structure matches topic requirements" $ do
            -- The proof should have the correct topic ID in its path
            let topicPath = pathTopicTriePath testProofTyped
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
              testProofTyped
              testTopHash
              @?= True
        ]
    ]
