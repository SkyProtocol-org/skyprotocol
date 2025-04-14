{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
--module Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec) where
module Spec.SkySpec where

import PlutusTx.Prelude -- hiding (Applicative, Functor, fmap, pure, (<*>))
import PlutusTx
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
import PlutusLedgerApi.V1.Interval (Interval(..), strictLowerBound, strictUpperBound)
import PlutusLedgerApi.V1.Time (POSIXTime(..))
import PlutusLedgerApi.V1.Value (CurrencySymbol(..))
import Data.Functor.Identity (Identity (..))
import Test.Hspec

import SkyBase
import SkyCrypto
import Trie
import SkyDA
import SkyBridgeContract
import BountyContract
import Spec.SkyBaseSpec

------------------------------------------------------------------------------
-- Single sigs
------------------------------------------------------------------------------
-- Keys generated via https://cyphr.me/ed25519_tool/ed.html

sk1, sk2, sk3 :: SecKey -- random seckeys, do not use outside of tests
sk1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
sk2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"
sk3 = ofHex "9F664160D9DDCD27B5B9A0C619FC3978DDE6C51F4FEAF40688BF54281AA0D0CC"

pk1, pk2, pk3 :: PubKey
pk1 = derivePubKey sk1
pk2 = derivePubKey sk2
pk3 = derivePubKey sk3

mpk1, mpk2 :: MultiSigPubKey
mpk1 = MultiSigPubKey ([pk1, pk2, pk3], toUInt16 2) -- Require 2 of the 3 pks to sign
mpk2 = MultiSigPubKey ([pk1, pk2], toUInt16 2) -- Require 2 of the 2 pks to sign

signatureSpec :: Spec
signatureSpec = do

  let fb1 = ofHex "CAFE" :: FixedLengthByteString L2
  let fb2 = ofHex "BABE" :: FixedLengthByteString L2

  let sig1 = signMessage sk1 fb1
  let ss1 = SingleSig (pk1, sig1)
  let sig2 = signMessage sk2 fb1
  let ss2 = SingleSig (pk2, sig2)
  let sig3 = signMessage sk3 fb1
  let ss3 = SingleSig (pk3, sig3)

  describe "check crypto primitives" $ do
    it "deriving pk's" $ do
      pk1 `shouldBeHex` "3363a313e34cf6d3b9e0ce44aed5a54567c4302b873dd69ec7f37b9e83aabf65"
      pk2 `shouldBeHex` "42fb07466d301ca2cc2eff2fd93a67eb1ebbec213e6532a04dc82be6a41329ae"
      pk3 `shouldBeHex` "22b9524d37a16c945deec3455d92a1ebc5ac857174f5a0a8b376517a205dca73"
    it "signing" $ do
      sig1 `shouldBeHex` "184e401e7ea7c7e2f9b4186dedf953437f81bd2664d1fbde525264a4d08bfd79d81877376f1e63ce64df46c5f1fd93cdf3b05b8b6076a6adc05f36c81f62a500"
      sig2 `shouldBeHex` "b7837207523b267f5b9aa0117c02773474a5f9f9fc4d6f48aeb2dc1b7a5796e60ee17c1f5c81d43c1973c0536932fb328897b341a7f8b3b86cb66acef459b405"
      sig3 `shouldBeHex` "2f1bc348540a34c6a049e590b03c8fc87d0a9aac213dff829a0bd4f9b46cbcaf744ae08676761eba38926a58aa60782b897a64295e3010339640e81eda74a20e"

  describe "Single Sig operations" $ do
    it "single sig 1 should be valid" $ do
      (singleSigValid fb1 ss1) `shouldBe` True
    it "single sig 2 should be valid" $ do
      (singleSigValid fb1 ss2) `shouldBe` True
    it "single sig 3 should be valid" $ do
      (singleSigValid fb1 ss3) `shouldBe` True
    it "single sig 1 should not be valid for wrong hash" $ do
      (singleSigValid fb2 ss1) `shouldBe` False
    it "single sig 2 should not be valid for wrong hash" $ do
      (singleSigValid fb2 ss2) `shouldBe` False
    it "single sig 3 should not be valid for wrong hash" $ do
      (singleSigValid fb2 ss3) `shouldBe` False

  ------------------------------------------------------------------------------
  -- Multi sigs
  ------------------------------------------------------------------------------

  let msig1OK = MultiSig [ss1, ss2]
  let msig2OK = MultiSig [ss1, ss2, ss3]
  let msig3OK = MultiSig [ss2, ss3]
  let msig4Err = MultiSig [] -- no sigs at all
  let msig5Err = MultiSig [ss1] -- too few sigs
  let msig6OK = MultiSig [ss1, ss2]
  let msig7Err = MultiSig [ss1, ss3] -- pk3 is not in mpk2

  describe "Multi Sig operations" $ do

    it "multi sig 1 should be valid" $ do
      (multiSigValid mpk1 fb1 msig1OK) `shouldBe` True

    it "multi sig 2 should be valid" $ do
      (multiSigValid mpk1 fb1 msig2OK) `shouldBe` True

    it "multi sig 3 should be valid" $ do
      (multiSigValid mpk1 fb1 msig3OK) `shouldBe` True

    it "multi sig 4 should be invalid" $ do
      (multiSigValid mpk1 fb1 msig4Err) `shouldBe` False

    it "multi sig 5 should be invalid" $ do
      (multiSigValid mpk1 fb1 msig5Err) `shouldBe` False

    it "multi sig 6 should be valid" $ do
      (multiSigValid mpk2 fb1 msig6OK) `shouldBe` True

    it "multi sig 7 should be invalid" $ do
      (multiSigValid mpk2 fb1 msig7Err) `shouldBe` False

    it "multi sig 3 should be invalid for wrong hash" $ do
      (multiSigValid mpk1 fb2 msig3OK) `shouldBe` False

    it "multi sig 6 should be invalid for wrong hash" $ do
      (multiSigValid mpk2 fb2 msig6OK) `shouldBe` False

------------------------------------------------------------------------------
-- Fingerprints
------------------------------------------------------------------------------

fingerprintSpec :: Spec
fingerprintSpec = do

    it "multi sig 1 serialization should match" $ do
      hexOf mpk1 `shouldBe` "00033363a313e34cf6d3b9e0ce44aed5a54567c4302b873dd69ec7f37b9e83aabf6542fb07466d301ca2cc2eff2fd93a67eb1ebbec213e6532a04dc82be6a41329ae22b9524d37a16c945deec3455d92a1ebc5ac857174f5a0a8b376517a205dca730002"

    it "multi sig 1 fingerprint should match" $ do
      hexOf (computeHash mpk1) `shouldBe` "6f25872869654adb946b83b82490b2f38c001212e6815f86f41134ffd05c8327"

    it "multi sig 2 fingerprint should match" $ do
      hexOf (computeHash mpk2) `shouldBe` "1e974300f36903173a25402220e346503bc747e4549b608543939566f74ffe83"


daSchema0 :: DataHash
daSchema0 = computeHash $ (ofHex "deadbeef" :: Bytes4)

topicSchema0 :: DataHash
topicSchema0 = computeHash $ (ofHex "1ea7f00d" :: Bytes4)

committee0 :: Committee
committee0 = MultiSigPubKey ([pk1, pk2], UInt16 2)

daMetaData0 :: DaMetaData HashRef
daMetaData0 = DaMetaData daSchema0 (LiftRef (digestRef committee0))

timestamp1 :: POSIXTime
timestamp1 = 455155200000 -- June 4th 1989

deadline :: POSIXTime
deadline = 1000180800000 -- Sep 11th 2001

txBeforeDeadlineRange :: Interval POSIXTime
txBeforeDeadlineRange = Interval (strictLowerBound 999316800000) -- Sep 1st 2001
                          (strictUpperBound 1000008000000) -- Sep 9th 2001

txAfterDeadlineRange :: Interval POSIXTime
txAfterDeadlineRange = Interval (strictLowerBound 1000267200000) -- Sep 12th 2001
                          (strictUpperBound 1000872000000) -- Sep 19th 2001

txAroundDeadlineRange :: Interval POSIXTime
txAroundDeadlineRange = Interval (strictLowerBound 1000008000000) -- Sep 9th 2001
                          (strictUpperBound 1000872000000) -- Sep 19th 2001

msgMeta1 :: MessageMetaData HashRef
msgMeta1 = MessageMetaData pk1 timestamp1

msg1 :: VariableLengthByteString
msg1 = VariableLengthByteString . stringToBuiltinByteString $ "Hello, World!"

msg1Hash :: DataHash
msg1Hash = computeHash msg1

msg2 :: VariableLengthByteString
msg2 = VariableLengthByteString . stringToBuiltinByteString $ "Taxation is Theft"

msg3 :: VariableLengthByteString
msg3 = VariableLengthByteString . stringToBuiltinByteString $ "Slava Drakonu"

msg3Hash :: DataHash
msg3Hash = computeHash msg3

daSpec :: Spec
daSpec = do
  describe "simple tests for SkyDA" $ do
    -- Create an empty SkyDA, check its digest
    let da0 = runIdentity $ initDa daSchema0 committee0 :: SkyDa HashRef
    let topHash0 = computeHash da0
    it "hash of empty DA" $ do
      computeHash da0 `shouldBeHex` "8fb3f562be2da84052ca81850060c549ac8b9914799885c8e1651405a3c38d19"

    -- Check that proofs come in empty
    let maybeProof0 = runIdentity $ getSkyDataProof (fromInt 0, fromInt 0) da0
          :: Maybe (_, SkyDataProof Blake2b_256)
    it "No proof for (0,0) in empty Da" $ do
      maybeProof0 `shouldBeHex` "00"

    let (Just topic0, da1) = runIdentity $ insertTopic topicSchema0 da0
    let (Just msg1i, da2) = runIdentity $ insertMessage pk1 timestamp1 msg1 topic0 da1
    let (Just msg2i, da3) = runIdentity $ insertMessage pk1 timestamp1 msg2 topic0 da2
    let Just (msg1b , proof1) = runIdentity $ getSkyDataProof (topic0, msg1i) da3
          :: Maybe (_, SkyDataProof Blake2b_256)
    let rMessageData1 = runIdentity $ wrap msg1 :: LiftRef HashRef (MessageData HashRef)
    it "msg1 matches" $ do
      msg1b == LiftRef (digestRef msg1) `shouldBe` True
    let l1d = (castDigest . getDigest . liftref $ msg1b) :: DataHash
    let topHash1 = computeHash da3 :: DataHash

    it "proof1 correct" $ do
      applySkyDataProof proof1 l1d == topHash1 `shouldBe` True
      (triePathHeight . pathTopicTriePath $ proof1) == 0 `shouldBe` True
      (triePathKey . pathTopicTriePath $ proof1) == topic0 `shouldBe` True
      (triePathHeight . pathMessageTriePath $ proof1) == 0 `shouldBe` True
      (triePathKey . pathMessageTriePath $ proof1) == msg1i `shouldBe` True

    ------------------------------------------------------------------------------
    -- Test Bounty Contract
    ------------------------------------------------------------------------------

    -- da10 has same committee as da3 but different root hash
    let (Just topic1, da10) = runIdentity $ insertTopic topicSchema0 da3
    let (Just msg3i, da11) = runIdentity $ insertMessage pk1 timestamp1 msg3 topic1 da10

    -- da20 has different committee as top hash 2 but same root hash
    let da10 = runIdentity $ updateDaCommittee mpk1 da11

    let Just (msg3b , proof3) = runIdentity $ getSkyDataProof (topic1, msg3i) da11
          :: Maybe (_, SkyDataProof Blake2b_256)
    let rMessageData1 = runIdentity $ wrap msg3 :: LiftRef HashRef (MessageData HashRef)
    it "msg3 matches" $ do
      msg3b == LiftRef (digestRef msg3) `shouldBe` True
    let l3d = (castDigest . getDigest . liftref $ msg3b) :: DataHash
    let topHash3 = computeHash da11 :: DataHash
    it "proof3 correct" $ do
      applySkyDataProof proof3 l3d == topHash3 `shouldBe` True
      (triePathHeight . pathTopicTriePath $ proof3) == 0 `shouldBe` True
      (triePathKey . pathTopicTriePath $ proof3) == topic1 `shouldBe` True
      (triePathHeight . pathMessageTriePath $ proof3) == 0 `shouldBe` True
      (triePathKey . pathMessageTriePath $ proof3) == msg3i `shouldBe` True

    it "hashes differ" $ do
      topHash1 == topHash3 `shouldBe` False
      topHash1 `shouldBeHex` "f889a9fec14bad5dfd59bd560a6d7626f11f048a194f813a8b090745ed243253"
      topHash3 `shouldBeHex` "c0bd8a731290df2fe149240c8271e56182b9d211ab8b9e5ba4ea7073af9dbc8a"

    it "Bounty contract should accept claim for msg1" $ do
      validateClaimBounty deadline txBeforeDeadlineRange msg1Hash topic0 proof1 topHash1 `shouldBe` True

    it "Bounty contract should accept claim for msg3" $ do
      validateClaimBounty deadline txBeforeDeadlineRange msg3Hash topic1 proof3 topHash3 `shouldBe` True

    it "contract should not accept claim for wrong topic" $ do
      validateClaimBounty deadline txBeforeDeadlineRange msg3Hash topic0 proof3 topHash3 `shouldBe` False

    it "contract should not accept claim for wrong data" $ do
      validateClaimBounty deadline txBeforeDeadlineRange msg3Hash topic1 proof1 topHash1 `shouldBe` False

    it "contract should not accept claim after deadline" $ do
      validateClaimBounty deadline txAfterDeadlineRange msg1Hash topic0 proof1 topHash1 `shouldBe` False

    it "contract should not accept claim in interval around deadline" $ do
      validateClaimBounty deadline txAroundDeadlineRange msg1Hash topic0 proof1 topHash1 `shouldBe` False

    it "contract should not accept claim with wrong proof" $ do
      validateClaimBounty deadline txBeforeDeadlineRange msg1Hash topic0 proof3 topHash1 `shouldBe` False

    it "contract should not accept claim with wrong top hash" $ do
      validateClaimBounty deadline txBeforeDeadlineRange msg1Hash topic0 proof1 topHash3 `shouldBe` False

    it "Bounty contract should accept timeout after deadline" $ do
      validateTimeout deadline txAfterDeadlineRange `shouldBe` True

    it "contract should reject timeout before deadline" $ do
      validateTimeout deadline txBeforeDeadlineRange `shouldBe` False

    it "contract should reject timeout in interval around deadline" $ do
      validateTimeout deadline txAroundDeadlineRange `shouldBe` False

    ------------------------------------------------------------------------------
    -- Bridge Contract
    ------------------------------------------------------------------------------

    -- topHash3 signed by sk1
    -- topHash3: c0bd8a731290df2fe149240c8271e56182b9d211ab8b9e5ba4ea7073af9dbc8a
    let topHash3Sig1 = SingleSig (pk1, signMessage sk1 (toByteString da3))
    let topHash3Sig2 = SingleSig (pk1, signMessage sk1 (toByteString da3))
    it "topHash3Sig1" $ do topHash3Sig1 `shouldBeHex` "3363a313e34cf6d3b9e0ce44aed5a54567c4302b873dd69ec7f37b9e83aabf65e857366dd21fe441630fd4442e981b255aa86a5f310aa7f655fbf51af170ca7b20df09324c8bc388d1c6ea6de06d18b0f1c90e6a7f33558f2e3425b147930008"
    it "topHash3Sig2" $ do topHash3Sig2 `shouldBeHex` "3363a313e34cf6d3b9e0ce44aed5a54567c4302b873dd69ec7f37b9e83aabf65e857366dd21fe441630fd4442e981b255aa86a5f310aa7f655fbf51af170ca7b20df09324c8bc388d1c6ea6de06d18b0f1c90e6a7f33558f2e3425b147930008"
    return ()
{-
-- top hash 2 signed by sk2
-- th2: 3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1
topHash2Sig2 :: SingleSig
topHash2Sig2 = SingleSig pk2 (hex "99E3BBBCA63ECDA27ADC6ED426A695E32AA5D7185CFC16F550834919C96F7FA17E19992E6FB2D302BE8FF71CF71907F654F25727425C0F30989B4AAC7767B003")

topHash2Sig :: MultiSig
topHash2Sig = MultiSig [topHash2Sig1, topHash2Sig2]

-- top hash 3 signed by sk1
-- th3: 9e0c40f42058194826884d1baf37c95bb916eebab55153461eed30e4f45042ce
topHash3Sig1 :: SingleSig
topHash3Sig1 = SingleSig pk1 (hex "9286F3C3FCA4ADC044013E343089829AB9EB68C96B407B7C4D400B63B8E9B5A551C4FACB948615ADAC3C47833FD1320BEA76B09A90D7B2982D511E1C4B6A010F")

-- top hash 3 signed by sk2
-- th3: 9e0c40f42058194826884d1baf37c95bb916eebab55153461eed30e4f45042ce
topHash3Sig2 :: SingleSig
topHash3Sig2 = SingleSig pk2 (hex "A5CF5C46F3DB920896E93ECBC726EECA8F488B6ED1A3E2D7122AE0E067DC9A1A24FC8EDD22DDDD329CFFB6791D5840BD0F95BF27463711F99225B5162719E20E")

topHash3Sig :: MultiSig
topHash3Sig = MultiSig [topHash3Sig1, topHash3Sig2]

bridgeSpec :: Spec
bridgeSpec = do

  it "topHash2Sig1 valid" $ do
    (singleSigValid topHash2 topHash2Sig1) `shouldBe` True

  it "topHash2Sig2 valid" $ do
    (singleSigValid topHash2 topHash2Sig2) `shouldBe` True

  it "topHash2Sig valid" $ do
    (multiSigValid mainCommitteePK topHash2 topHash2Sig) `shouldBe` True

  it "topHash3Sig1 valid" $ do
    (singleSigValid topHash3 topHash3Sig1) `shouldBe` True

  it "topHash3Sig2 valid" $ do
    (singleSigValid topHash3 topHash3Sig2) `shouldBe` True

  it "topHash3Sig valid" $ do
    (multiSigValid mainCommitteePK topHash3 topHash3Sig) `shouldBe` True

  it "bridge accepts top hash 2" $ do
    (bridgeTypedValidatorCore mainCommitteePK mainRootHash1 topHash2 topHash2Sig topHash1) `shouldBe` True

  it "bridge accepts top hash 3" $ do
    (bridgeTypedValidatorCore mainCommitteePK mainRootHash1 topHash3 topHash3Sig topHash1) `shouldBe` True

  it "bridge doesn't accept top hash 2 with wrong committee" $ do
    (bridgeTypedValidatorCore topic1CommitteePK mainRootHash1 topHash2 topHash2Sig topHash1) `shouldBe` False

  it "bridge doesn't accept top hash 2 with wrong old root hash" $ do
    (bridgeTypedValidatorCore mainCommitteePK mainRootHash2 topHash2 topHash2Sig topHash1) `shouldBe` False

  it "bridge doesn't accept top hash 2 with wrong signature" $ do
    (bridgeTypedValidatorCore mainCommitteePK mainRootHash1 topHash2 msig1OK topHash1) `shouldBe` False

  it "bridge doesn't accept top hash 2 with wrong old top hash" $ do
    (bridgeTypedValidatorCore mainCommitteePK mainRootHash1 topHash2 topHash2Sig dh1) `shouldBe` False
-}
