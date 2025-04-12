{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures #-}
--module Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec) where
module Spec.SkySpec where

import PlutusTx.Prelude -- hiding (Applicative, Functor, fmap, pure, (<*>))
import PlutusTx
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
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

dh1 :: DataHash -- blake2b_256 for "Hello, World!"
dh1 = ofHex "511bc81dde11180838c562c82bb35f3223f46061ebde4a955c27b3f489cf1e03"

dh2 :: DataHash -- blake2b_256 for "Taxation is Theft"
dh2 = ofHex "f7fc5335c31d34a0c362acf0e751716a029fa4ea152e3c3a27305a4c6d5e02ed"

fb1 :: FixedLengthByteString L2
fb1 = ofHex "CAFE"

fb2 :: FixedLengthByteString L2
fb2 = ofHex "BABE"

------------------------------------------------------------------------------
-- Single sigs
------------------------------------------------------------------------------
-- Keys generated via https://cyphr.me/ed25519_tool/ed.html

-- sk1: A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E
pk1 :: PubKey
pk1 = ofHex "3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF65"

sig1 :: Bytes64 -- signs dh1 with sk1
sig1 = ofHex "184E401E7EA7C7E2F9B4186DEDF953437F81BD2664D1FBDE525264A4D08BFD79D81877376F1E63CE64DF46C5F1FD93CDF3B05B8B6076A6ADC05F36C81F62A500"

ss1 :: SingleSig
ss1 = SingleSig (pk1, sig1)

-- sk2: B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F
pk2 :: PubKey
pk2 = ofHex "42FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE"

sig2 :: Bytes64 -- signs dh1 with sk2
sig2 = ofHex "B7837207523B267F5B9AA0117C02773474A5F9F9FC4D6F48AEB2DC1B7A5796E60EE17C1F5C81D43C1973C0536932FB328897B341A7F8B3B86CB66ACEF459B405"

ss2 :: SingleSig
ss2 = SingleSig (pk2, sig2)

-- sk3: 9F664160D9DDCD27B5B9A0C619FC3978DDE6C51F4FEAF40688BF54281AA0D0CC
pk3 :: PubKey
pk3 = ofHex "22B9524D37A16C945DEEC3455D92A1EBC5AC857174F5A0A8B376517A205DCA73"

sig3 :: Bytes64 -- signs fb1 with sk3
sig3 = ofHex "2F1BC348540A34C6A049E590B03C8FC87D0A9AAC213DFF829A0BD4F9B46CBCAF744AE08676761EBA38926A58AA60782B897A64295E3010339640E81EDA74A20E"

ss3 :: SingleSig
ss3 = SingleSig (pk3, sig3)

------------------------------------------------------------------------------
-- Multi sigs
------------------------------------------------------------------------------

mpk1 :: MultiSigPubKey -- Require 2 of the 3 pks to sign
mpk1 = MultiSigPubKey ([pk1, pk2, pk3], toUInt16 2)

msig1OK :: MultiSig
msig1OK = MultiSig [ss1, ss2]

msig2OK :: MultiSig
msig2OK = MultiSig [ss1, ss2, ss3]

msig3OK :: MultiSig
msig3OK = MultiSig [ss2, ss3]

msig4Err :: MultiSig -- no sigs at all
msig4Err = MultiSig []

msig5Err :: MultiSig -- too few sigs
msig5Err = MultiSig [ss1]

mpk2 :: MultiSigPubKey -- Require 2 of the 2 pks to sign
mpk2 = MultiSigPubKey ([pk1, pk2], toUInt16 2)

msig6OK :: MultiSig
msig6OK = MultiSig [ss1, ss2]

msig7Err :: MultiSig -- pk3 is not in mpk2
msig7Err = MultiSig [ss1, ss3]

signatureSpec :: Spec
signatureSpec = do

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

------------------------------------------------------------------------------
-- Merkle Proof
------------------------------------------------------------------------------

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

msgMeta1 :: MessageMetaData HashRef
msgMeta1 = MessageMetaData pk1 timestamp1

msg1 :: VariableLengthByteString
msg1 = VariableLengthByteString . stringToBuiltinByteString $ "Hello, World!"

msg2 :: VariableLengthByteString
msg2 = VariableLengthByteString . stringToBuiltinByteString $ "Taxation is Theft"

msg3 :: VariableLengthByteString
msg3 = VariableLengthByteString . stringToBuiltinByteString $ "Slava Drakonu"

daSpec :: Spec
daSpec = do
  describe "simple tests for SkyDA" $ do
    -- Create an empty SkyDA, check its digest
    let da0 = runIdentity $ initDa daSchema0 committee0 :: SkyDa HashRef
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
    it "proof1 correct" $ do
      applySkyDataProof proof1 l1d == castDigest (computeDigest da3) `shouldBe` True
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
      msg1b == LiftRef (digestRef msg3) `shouldBe` True
    let l3d = (castDigest . getDigest . liftref $ msg3b) :: DataHash
    it "proof3 correct" $ do
      applySkyDataProof proof3 l3d == castDigest (computeDigest da11) `shouldBe` True
      (triePathHeight . pathTopicTriePath $ proof3) == 0 `shouldBe` True
      (triePathKey . pathTopicTriePath $ proof3) == topic1 `shouldBe` True
      (triePathHeight . pathMessageTriePath $ proof3) == 0 `shouldBe` True
      (triePathKey . pathMessageTriePath $ proof3) == msg3i `shouldBe` True

    it "hashes differ" $ do
      (castDigest (computeDigest da3) :: DataHash) == (castDigest (computeDigest da3)) `shouldBe` False

{-
TODO: reenable
it "contract should accept claim for dh1" $ do
    clientTypedValidatorCore (ClaimBounty proof1 topicInDAProof1 topic1CommitteeFP mainCommitteeFP) topic1 dh1 topHash1 `shouldBe` True

  it "contract should accept claim for dh2" $ do
    clientTypedValidatorCore (ClaimBounty proof1 topicInDAProof1 topic1CommitteeFP mainCommitteeFP) topic1 dh2 topHash1 `shouldBe` True

  it "contract should not accept claim for wrong topic" $ do
    clientTypedValidatorCore (ClaimBounty proof1 topicInDAProof1 topic1CommitteeFP mainCommitteeFP) topic2 dh1 topHash1 `shouldBe` False

  it "contract should not accept claim for wrong data in topic proof" $ do
    clientTypedValidatorCore (ClaimBounty (SimplifiedMerkleProof topHash1 topHash1) topicInDAProof1 topic1CommitteeFP mainCommitteeFP) topic1 dh1 topHash1 `shouldBe` False

  it "contract should not accept claim for wrong topic in DA proof" $ do
    clientTypedValidatorCore (ClaimBounty proof1 (SimplifiedMerkleProof topHash1 topHash1) topic1CommitteeFP mainCommitteeFP) topic1 dh1 topHash1 `shouldBe` False

  it "contract should not accept claim for wrong topic committee" $ do
    clientTypedValidatorCore (ClaimBounty proof1 topicInDAProof1 mainCommitteeFP mainCommitteeFP) topic1 dh1 topHash1 `shouldBe` False

  it "contract should not accept claim for wrong main committee" $ do
    clientTypedValidatorCore (ClaimBounty proof1 topicInDAProof1 topic1CommitteeFP topic1CommitteeFP) topic1 dh1 topHash1 `shouldBe` False

  it "contract should not accept claim for wrong top hash" $ do
    clientTypedValidatorCore (ClaimBounty proof1 topicInDAProof1 topic1CommitteeFP mainCommitteeFP) topic1 dh1 dh1 `shouldBe` False

------------------------------------------------------------------------------
-- Bridge Contract
------------------------------------------------------------------------------

mainCommitteePK, topic1CommitteePK :: MultiSigPubKey
mainCommitteePK = mpk1
topic1CommitteePK = mpk2

-- top hash 2 signed by sk1
-- th2: 3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1
-- sk1: A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E
topHash2Sig1 :: SingleSig
topHash2Sig1 = SingleSig pk1 (hex "87E894C503E40A8CB98DEB8618DC068323092871C717D4781D56FCBBE10FCD6B1965ADE766FFDFAF8F7B2964F3ED8A6066703DD9AA68F583055ED53FBA27A90E")

-- top hash 2 signed by sk2
-- th2: 3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1
-- sk2: B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F
topHash2Sig2 :: SingleSig
topHash2Sig2 = SingleSig pk2 (hex "99E3BBBCA63ECDA27ADC6ED426A695E32AA5D7185CFC16F550834919C96F7FA17E19992E6FB2D302BE8FF71CF71907F654F25727425C0F30989B4AAC7767B003")

topHash2Sig :: MultiSig
topHash2Sig = MultiSig [topHash2Sig1, topHash2Sig2]

-- top hash 3 signed by sk1
-- th3: 9e0c40f42058194826884d1baf37c95bb916eebab55153461eed30e4f45042ce
-- sk1: A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E
topHash3Sig1 :: SingleSig
topHash3Sig1 = SingleSig pk1 (hex "9286F3C3FCA4ADC044013E343089829AB9EB68C96B407B7C4D400B63B8E9B5A551C4FACB948615ADAC3C47833FD1320BEA76B09A90D7B2982D511E1C4B6A010F")

-- top hash 3 signed by sk2
-- th3: 9e0c40f42058194826884d1baf37c95bb916eebab55153461eed30e4f45042ce
-- sk2: B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F
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
