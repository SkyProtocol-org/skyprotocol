--module Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec) where
module Spec.SkySpec where

import Test.Hspec
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
import PlutusLedgerApi.V1.Time (POSIXTime(..))
import PlutusLedgerApi.V1.Value (CurrencySymbol(..))
import Data.Text (pack)
import Data.Functor.Identity (Identity (..))
import Text.Hex (Text, ByteString, decodeHex)

import SkyBridgeContract
import BountyContract
import SkyBase
import SkyDA
import Trie

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

hex :: [Char] -> BuiltinByteString
hex s = fromJust (hexStringToBuiltinByteString (pack s))

dhex :: [Char] -> DataHash
dhex s = Digest . FixedLengthByteString $ hex s

dh1 :: DataHash -- blake2b_256 for "Hello, World!"
dh1 = Digest . FixedLengthByteString $ hex "511bc81dde11180838c562c82bb35f3223f46061ebde4a955c27b3f489cf1e03"

dh2 :: DataHash -- blake2b_256 for "Taxation is Theft"
dh2 = Digest . FixedLengthByteString $ hex "f7fc5335c31d34a0c362acf0e751716a029fa4ea152e3c3a27305a4c6d5e02ed"

------------------------------------------------------------------------------
-- Single sigs
------------------------------------------------------------------------------
-- Keys generated via https://cyphr.me/ed25519_tool/ed.html

-- sk1: A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E
pk1 :: PubKey
pk1 = PubKey . FixedLengthByteString . hex $ "3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF65"

sig1 :: Bytes64 -- signs dh1 with sk1
sig1 = FixedLengthByteString . hex $ "184E401E7EA7C7E2F9B4186DEDF953437F81BD2664D1FBDE525264A4D08BFD79D81877376F1E63CE64DF46C5F1FD93CDF3B05B8B6076A6ADC05F36C81F62A500"

ss1 :: SingleSig
ss1 = SingleSig pk1 sig1

-- sk2: B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F
pk2 :: PubKey
pk2 = PubKey . FixedLengthByteString . hex $ "42FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE"

sig2 :: Bytes64 -- signs dh1 with sk2
sig2 = FixedLengthByteString . hex $ "B7837207523B267F5B9AA0117C02773474A5F9F9FC4D6F48AEB2DC1B7A5796E60EE17C1F5C81D43C1973C0536932FB328897B341A7F8B3B86CB66ACEF459B405"

ss2 :: SingleSig
ss2 = SingleSig pk2 sig2

-- sk3: 9F664160D9DDCD27B5B9A0C619FC3978DDE6C51F4FEAF40688BF54281AA0D0CC
pk3 :: PubKey
pk3 = PubKey . FixedLengthByteString . hex $ "22B9524D37A16C945DEEC3455D92A1EBC5AC857174F5A0A8B376517A205DCA73"

sig3 :: Bytes64 -- signs dh1 with sk3
sig3 = FixedLengthByteString . hex $ "2F1BC348540A34C6A049E590B03C8FC87D0A9AAC213DFF829A0BD4F9B46CBCAF744AE08676761EBA38926A58AA60782B897A64295E3010339640E81EDA74A20E"

ss3 :: SingleSig
ss3 = SingleSig pk3 sig3

------------------------------------------------------------------------------
-- Multi sigs
------------------------------------------------------------------------------

mpk1 :: MultiSigPubKey -- Require 2 of the 3 pks to sign
mpk1 = MultiSigPubKey [pk1, pk2, pk3] (toUInt16 2)

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
mpk2 = MultiSigPubKey [pk1, pk2] (toUInt16 2)

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
mfp1 = Digest . FixedLengthByteString $ hex "5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f"

-- Concatenation of pk1, pk2:
-- 3363A313E34CF6D3B9E0CE44AED5A54567C4302B873DD69EC7F37B9E83AABF6542FB07466D301CA2CC2EFF2FD93A67EB1EBBEC213E6532A04DC82BE6A41329AE
-- sha256: b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767

mfp2 :: DataHash
mfp2 = Digest . FixedLengthByteString $ hex "b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767"


fingerprintSpec :: Spec
fingerprintSpec = do

    it "multi sig 1 fingerprint should match" $ do
      toByteString (multiSigToDataHash mpk1) `shouldBe` toByteString mfp1

    it "multi sig 2 fingerprint should match" $ do
      toByteString (multiSigToDataHash mpk2) `shouldBe` toByteString mfp2

------------------------------------------------------------------------------
-- Merkle Proof
------------------------------------------------------------------------------

committee0 :: Committee
committee0 = MultiSigPubKey [pk1, pk2] (UInt16 2)

timestamp1 :: POSIXTime
timestamp1 = 455155200000 -- June 4th 1989

msgMeta1 :: MessageMetaData
msgMeta1 = MessageMetaData pk1 timestamp1

msg1 :: VariableLengthByteString
msg1 = VariableLengthByteString . stringToBuiltinByteString $ "Hello, World!"

msg2 :: VariableLengthByteString
msg2 = VariableLengthByteString . stringToBuiltinByteString $ "Taxation is Theft"

topicMeta42 :: TopicMetaData
topicMeta42 = committee0

topic42 :: TopicEntry
topic42 = (digestRef topicMeta42,
           digestRef . runIdentity $ ofList
            [(fromInt 1,(digestRef msgMeta1, digestRef msg1))
            ,(fromInt 2,(digestRef msgMeta1, digestRef msg2))])

skyData1 :: SkyData
skyData1 = (digestRef committee0,
            digestRef . runIdentity $ ofList
             [(fromInt 42, topic42)])


daSpec :: Spec
daSpec = do
  return ()
{-
  it "should generate a proof, validate it, and compute the root hash correctly" $ do
    let t1 = runIdentity $ olt [(1,"value1"),(2,"value2")]
    let t1d :: Digest Blake2b_256 = computeDigest t1
    let proof1 = runIdentity $ getMerkleProof 1 t1
    let l1d :: Digest Blake2b_256 = getDigest . lifted . runIdentity $ ((rf $ Leaf "value1") :: Identity TR)
    let v1 = runIdentity $ isMerkleProof 1 l1d t1d proof1
    v1 `shouldBe` True
    let t0 = runIdentity $ olt []
    let t2 = runIdentity $ olt initialValues
    let u :: U = runIdentity $ ofList [(42,("foo",t1)),(17,("bar",t2)),(0,("",t0))]
    let ud :: Digest Blake2b_256 = computeDigest u
    let proof2 = runIdentity $ getMerkleProof 42 u
    let l2d :: Digest Blake2b_256 = getDigest . lifted . runIdentity $ ((rf $ Leaf ("foo", t1)) :: Identity UR)
    let v2 = runIdentity $ isMerkleProof 42 l2d ud proof2
    v2 `shouldBe` True
    -- should fail to validate an incorrect proof
    let l1d' :: Digest Blake2b_256 = getDigest . lifted . runIdentity $ ((rf $ Leaf "value3") :: Identity TR)
    let v1' = runIdentity $ isMerkleProof 1 l1d' t1d proof1
    v1' `shouldBe` False



proof1 :: SkyDataProof
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
    toBytesString rootHash1 `shouldBe` hex "65ab12a8ff3263fbc257e5ddf0aa563c64573d0bab1f1115b9b107834cfa6971"
-}
{-
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
    toByteString topic1TopHash `shouldBe` hex "5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f"

------------------------------------------------------------------------------
-- Bounty Contract
------------------------------------------------------------------------------

topic2TopHash :: DataHash
topic2TopHash = Digest . FixedLengthByteString $ hex "0000"

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

-- Top hash 3 has different committee as top hash 2 but same root hash

topHash3 :: DataHash
topHash3 = pairHash topic1CommitteeFP mainRootHash1

bountySpec :: Spec
bountySpec = do

  it "valid merkle proofs should be accepted 1" $ do
    let proof = MerkleProof
          { targetKey = hex "02"
          , keySize = 256
          , keyPath = [0]
          , siblingHashes = [dhex "8CA3CA37EFDBFEA80767D1D88BC1E52DCD7620D40A2135875358F85292514126"]
          }
    let targetHash = dhex "92DB047787B7FCAFB4211D1AE970DD1CA6FA57DA7D5590D489B9521D3898187C"
    let rootHash = dhex "9C6239944C0A848E327EF1A7E52DA1AB00281E37A74561786949DB708D45B369"
    validate rootHash proof targetHash `shouldBe` True

  it "valid merkle proofs should be accepted 2" $ do
    let doubleProof = MerkleProof
          { targetKey = hex "01"
          , keySize = 256
          , keyPath = [0]
          , siblingHashes = [dhex "303D2543F7E1AEEF893A9DC0C097A5A1C55522D73855BB597C36C94A7D99F5AF"]
          }
    let targetHash = dhex "AADDAD8B4DA8F0A58CBF948CA41553DB039285D3A4C207B46F3C5F95FB28449A"
    let rootHash = dhex "AAA668EACAC5BD082FACE0281D63849C422F1FB776B6D17365F55A2DA432E188"
    validate rootHash doubleProof targetHash `shouldBe` True

  it "invalid merkle proofs should not be accepted" $ do
    let invalidProof = MerkleProof
          { targetKey = hex "01"
          , keySize = 256
          , keyPath = [0]
          , siblingHashes = [dhex "92DB047787B7FCAFB4211D1AE970DD1CA6FA57DA7D5590D489B9521D3898187C"]
          }
    let targetHash = dhex "8CA3CA37EFDBFEA80767D1D88BC1E52DCD7620D40A2135875358F85292514126"
    let rootHash = dhex "9C6239944C0A848E327EF1A7E52DA1AB00281E37A74561786949DB708D45B369"
    validate rootHash invalidProof targetHash `shouldBe` True

  it "main root hash 1 should be correct" $ do
    -- Sha256 of concatenation of topic1TopHash ++ topic2TopHash
    -- 5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f ++ 0000
    toByteString mainRootHash1 `shouldBe` hex "9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263"

  it "main root hash 2 should be correct" $ do
    -- Sha256 of concatenation of topic2TopHash ++ topic1TopHash
    -- 0000 ++ 5c82f057ac60bbc4c347d15418960d453468ffa2b6f8b2e0041d0cad3453f67f
    toByteString mainRootHash2 `shouldBe` hex "9445c184e34e8e672e574e51141b1a88df56f692598811a3c31aab6d6727a10f"

  it "top hash 1 should be correct" $ do
    -- Sha256 of concatenation of mainCommitteeFP ++ mainRootHash1
    -- 5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f
    -- ++ 9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263
    toByteString topHash1 `shouldBe` hex "41f011893595e8cf96f9effee819310d41f9038c7adfb0d3d7b1b5ddfaac6710"

  it "top hash 2 should be correct" $ do
    -- Sha256 of concatenation of mainCommitteeFP ++ mainRootHash2
    -- 5470fbfd926cdaa4ffc4d9d186670b37c35a3055875fbcaac403d0a3cf86df9f
    -- ++ 9445c184e34e8e672e574e51141b1a88df56f692598811a3c31aab6d6727a10f
    toByteString topHash2 `shouldBe` hex "3c7dfafe47aac5454629d9280529b90b82d07ba80b89757d652bff047f0534a1"

  it "top hash 3 should be correct" $ do
    -- Sha256 of concatenation of topic1CommitteeFP ++ mainRootHash1
    -- b25f003443ff6eb36a6baafaf5bc5d5e78c1dbd4533e3c49be498f23a9ac5767
    -- ++ 9f06268167a61b7f54210ebcd0a92d9000211a41401f7827b5bf905b8fd3e263
    toByteString topHash3 `shouldBe` hex "9e0c40f42058194826884d1baf37c95bb916eebab55153461eed30e4f45042ce"

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
-}
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

