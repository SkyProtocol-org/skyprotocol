module Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec) where

import Test.Hspec
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import Text.Hex (Text, ByteString, decodeHex)
import Data.Text (pack)
import Data.Maybe (fromJust)
import SkyBridgeContract
import BountyContract

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

hex :: [Char] -> BuiltinByteString
hex s = fromJust (hexStringToBuiltinByteString (pack s))

bytes :: DataHash -> BuiltinByteString
bytes (DataHash bytes) = bytes

-- Keys generated via https://cyphr.me/ed25519_tool/ed.html

dh1 :: DataHash
dh1 = DataHash $ hex "CAFE"

dh2 :: DataHash
dh2 = DataHash $ hex "BABE"

------------------------------------------------------------------------------
-- Single sigs
------------------------------------------------------------------------------

-- sk1: A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E
pk1 :: PubKey
pk1 = PubKey $ hex "3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF65"

sig1 :: BuiltinByteString -- signs dh1 with sk1
sig1 = hex "184E401E7EA7C7E2F9B4186DEDF953437F81BD2664D1FBDE525264A4D08BFD79D81877376F1E63CE64DF46C5F1FD93CDF3B05B8B6076A6ADC05F36C81F62A500"

ss1 :: SingleSig
ss1 = SingleSig pk1 sig1

-- sk2: B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F
pk2 :: PubKey
pk2 = PubKey $ hex "42FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE"

sig2 :: BuiltinByteString -- signs dh1 with sk2
sig2 = hex "B7837207523B267F5B9AA0117C02773474A5F9F9FC4D6F48AEB2DC1B7A5796E60EE17C1F5C81D43C1973C0536932FB328897B341A7F8B3B86CB66ACEF459B405"

ss2 :: SingleSig
ss2 = SingleSig pk2 sig2

-- sk3: 9F664160D9DDCD27B5B9A0C619FC3978DDE6C51F4FEAF40688BF54281AA0D0CC
pk3 :: PubKey
pk3 = PubKey $ hex "22B9524D37A16C945DEEC3455D92A1EBC5AC857174F5A0A8B376517A205DCA73"

sig3 :: BuiltinByteString -- signs dh1 with sk3
sig3 = hex "2F1BC348540A34C6A049E590B03C8FC87D0A9AAC213DFF829A0BD4F9B46CBCAF744AE08676761EBA38926A58AA60782B897A64295E3010339640E81EDA74A20E"

ss3 :: SingleSig
ss3 = SingleSig pk3 sig3

------------------------------------------------------------------------------
-- Multi sigs
------------------------------------------------------------------------------

mpk1 :: MultiSigPubKey -- Require 2 of the 3 pks to sign
mpk1 = MultiSigPubKey [pk1, pk2, pk3] 2

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
mpk2 = MultiSigPubKey [pk1, pk2] 2

msig6OK :: MultiSig
msig6OK = MultiSig [ss1, ss2]

msig7Err :: MultiSig -- pk3 is not in mpk2
msig7Err = MultiSig [ss1, ss3]

signatureSpec :: Spec
signatureSpec = do

  describe "Single Sig operations" $ do

    it "single sig 1 should be valid" $ do
      (singleSigValid dh1 ss1) `shouldBe` True

    it "single sig 2 should be valid" $ do
      (singleSigValid dh1 ss2) `shouldBe` True

    it "single sig 3 should be valid" $ do
      (singleSigValid dh1 ss3) `shouldBe` True

    it "single sig 1 should not be valid for wrong hash" $ do
      (singleSigValid dh2 ss1) `shouldBe` False

    it "single sig 2 should not be valid for wrong hash" $ do
      (singleSigValid dh2 ss2) `shouldBe` False

    it "single sig 3 should not be valid for wrong hash" $ do
      (singleSigValid dh2 ss3) `shouldBe` False

  describe "Multi Sig operations" $ do

    it "multi sig 1 should be valid" $ do
      (multiSigValid mpk1 dh1 msig1OK) `shouldBe` True

    it "multi sig 2 should be valid" $ do
      (multiSigValid mpk1 dh1 msig2OK) `shouldBe` True

    it "multi sig 3 should be valid" $ do
      (multiSigValid mpk1 dh1 msig3OK) `shouldBe` True

    it "multi sig 4 should be invalid" $ do
      (multiSigValid mpk1 dh1 msig4Err) `shouldBe` False

    it "multi sig 5 should be invalid" $ do
      (multiSigValid mpk1 dh1 msig5Err) `shouldBe` False

    it "multi sig 6 should be valid" $ do
      (multiSigValid mpk2 dh1 msig6OK) `shouldBe` True

    it "multi sig 7 should be invalid" $ do
      (multiSigValid mpk2 dh1 msig7Err) `shouldBe` False

    it "multi sig 3 should be invalid for wrong hash" $ do
      (multiSigValid mpk1 dh2 msig3OK) `shouldBe` False

    it "multi sig 6 should be invalid for wrong hash" $ do
      (multiSigValid mpk2 dh2 msig6OK) `shouldBe` False

------------------------------------------------------------------------------
-- Fingerprints
------------------------------------------------------------------------------

-- Concatenation of pk1, pk2, pk3:
-- 3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF6542FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE22B9524D37A16C945DEEC3455D92A1EBC5AC857174F5A0A8B376517A205DCA73
-- sha256: 5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f

mfp1 :: DataHash
mfp1 = DataHash $ hex "5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f"

-- Concatenation of pk1, pk2:
-- 3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF6542FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE
-- sha256: b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767

mfp2 :: DataHash
mfp2 = DataHash $ hex "b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767"

fingerprintSpec :: Spec
fingerprintSpec = do

    it "multi sig 1 fingerprint should match" $ do
      bytes (multiSigToDataHash mpk1) `shouldBe` bytes mfp1

    it "multi sig 2 fingerprint should match" $ do
      bytes (multiSigToDataHash mpk2) `shouldBe` bytes mfp2

------------------------------------------------------------------------------
-- Merkle Proof
------------------------------------------------------------------------------

proof1 :: SimplifiedMerkleProof
proof1 = SimplifiedMerkleProof dh1 dh2

rootHash1 :: DataHash
rootHash1 = merkleProofToDataHash proof1

merkleSpec :: Spec
merkleSpec = do

  it "hash 1 should be in proof" $ do
    hashInMerkleProof proof1 dh1 `shouldBe` True

  it "hash 2 should be in proof" $ do
    hashInMerkleProof proof1 dh2 `shouldBe` True

  it "other hash should not be in proof" $ do
    hashInMerkleProof proof1 mfp1 `shouldBe` False

  it "proof root hash should be concatenation of hashes" $ do
    -- sha256 of dh1 ++ dh2: CAFEBABE
    bytes rootHash1 `shouldBe` hex "65ab12a8ff3263fbc257e5ddf0aa563c64573d0bab1f1115b9b107834cfa6971"

------------------------------------------------------------------------------
-- Topic Top Hash
------------------------------------------------------------------------------

topic1 :: TopicID
topic1 = TopicID $ hex "00"

topic2 :: TopicID
topic2 = TopicID $ hex "01"

topic1CommitteeFP :: DataHash
topic1CommitteeFP = mfp2

topic1TopHash :: DataHash
topic1TopHash = makeTopicTopHash topic1 topic1CommitteeFP rootHash1

topicSpec :: Spec
topicSpec = do

  it "topic 1 top hash should be correct" $ do
    -- Sha256 of concatenation of topic1 ++ topic1CommitteeFP ++ rootHash1:
    -- 00 ++ b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767 ++ 65ab12a8ff3263fbc257e5ddf0aa563c64573d0bab1f1115b9b107834cfa6971
    bytes topic1TopHash `shouldBe` hex "5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f"

------------------------------------------------------------------------------
-- Bounty Contract
------------------------------------------------------------------------------

topic2TopHash :: DataHash
topic2TopHash = DataHash $ hex "0000"

mainCommitteeFP :: DataHash
mainCommitteeFP = mfp1

topicInDAProof1 :: SimplifiedMerkleProof
topicInDAProof1 = SimplifiedMerkleProof topic1TopHash topic2TopHash

mainRootHash1 :: DataHash
mainRootHash1 = merkleProofToDataHash topicInDAProof1

topHash1 :: DataHash
topHash1 = pairHash mainCommitteeFP mainRootHash1

-- Top hash 2 has same committee as top hash 1 but different root hash

topicInDAProof2 :: SimplifiedMerkleProof
topicInDAProof2 = SimplifiedMerkleProof topic2TopHash topic1TopHash -- switch order

mainRootHash2 :: DataHash
mainRootHash2 = merkleProofToDataHash topicInDAProof2

topHash2 :: DataHash
topHash2 = pairHash mainCommitteeFP mainRootHash2

bountySpec :: Spec
bountySpec = do

  it "main root hash 1 should be correct" $ do
    -- Sha256 of concatenation of topic1TopHash ++ topic2TopHash
    -- 5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f ++ 0000
    bytes mainRootHash1 `shouldBe` hex "9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263"

  it "main root hash 2 should be correct" $ do
    -- Sha256 of concatenation of topic2TopHash ++ topic1TopHash
    -- 0000 ++ 5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f
    bytes mainRootHash2 `shouldBe` hex "9445c184e34e8e672e574e51141b1a88df56f692598811a3c31aab6d6727a10f"

  it "top hash 1 should be correct" $ do
    -- Sha256 of concatenation of mainCommitteeFP ++ mainRootHash1
    -- 5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f
    -- ++ 9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263
    bytes topHash1 `shouldBe` hex "41f011893595e8cf96f9effee819310d41f9038c7adfb0d3d7b1b5ddfaac6710"

  it "top hash 2 should be correct" $ do
    -- Sha256 of concatenation of mainCommitteeFP ++ mainRootHash2
    -- 5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f
    -- ++ 41f011893595e8cf96f9effee819310d41f9038c7adfb0d3d7b1b5ddfaac6710
    bytes topHash2 `shouldBe` hex "33aeb326ed6f13fbeb6e8b13726caa378ac8d31fa8b67ecf93584c1bcaef18d8"

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

-- Top hash 3 has different committee as top hash 1 but same root hash

topHash3 :: DataHash
topHash3 = pairHash topic1CommitteeFP mainRootHash1

bridgeSpec :: Spec
bridgeSpec = return

--  it "bridge should accept update of top hash" $ do
--    bridgeTypedValidatorCore (UpdateBridge mpk1 mainRootHash1 topHash2 ) topic1 dh1 dh1 `shouldBe` False
