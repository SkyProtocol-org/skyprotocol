{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Common.DaSpec where

import Common.Crypto
import Common.DA
import Common.Trie
import Common.Types
import Common.TypesSpec
import Data.Functor.Identity (Identity (..))
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit

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

signatureSpec :: TestTree
signatureSpec = testGroup "Signature Tests"
  let fb1 = ofHex "CAFE" :: FixedLengthByteString L2
      fb2 = ofHex "BABE" :: FixedLengthByteString L2
      sig1 = signMessage sk1 fb1
      ss1 = SingleSig (pk1, sig1)
      sig2 = signMessage sk2 fb1
      ss2 = SingleSig (pk2, sig2)
      sig3 = signMessage sk3 fb1
      ss3 = SingleSig (pk3, sig3)
  in
  [ testGroup "check crypto primitives"
    [ testCase "deriving pk's" $ do
        pk1 `shouldBeHex` "3363a313e34cf6d3b9e0ce44aed5a54567c4302b873dd69ec7f37b9e83aabf65"
        pk2 `shouldBeHex` "42fb07466d301ca2cc2eff2fd93a67eb1ebbec213e6532a04dc82be6a41329ae"
        pk3 `shouldBeHex` "22b9524d37a16c945deec3455d92a1ebc5ac857174f5a0a8b376517a205dca73"
    , testCase "signing" $ do
        sig1 `shouldBeHex` "184e401e7ea7c7e2f9b4186dedf953437f81bd2664d1fbde525264a4d08bfd79d81877376f1e63ce64df46c5f1fd93cdf3b05b8b6076a6adc05f36c81f62a500"
        sig2 `shouldBeHex` "b7837207523b267f5b9aa0117c02773474a5f9f9fc4d6f48aeb2dc1b7a5796e60ee17c1f5c81d43c1973c0536932fb328897b341a7f8b3b86cb66acef459b405"
        sig3 `shouldBeHex` "2f1bc348540a34c6a049e590b03c8fc87d0a9aac213dff829a0bd4f9b46cbcaf744ae08676761eba38926a58aa60782b897a64295e3010339640e81eda74a20e"
    ]

  , testGroup "Single Sig operations"
    [ testCase "single sig 1 should be valid" $ do
        singleSigValid fb1 ss1 @?= True
    , testCase "single sig 2 should be valid" $ do
        singleSigValid fb1 ss2 @?= True
    , testCase "single sig 3 should be valid" $ do
        singleSigValid fb1 ss3 @?= True
    , testCase "single sig 1 should not be valid for wrong hash" $ do
        singleSigValid fb2 ss1 @?= False
    , testCase "single sig 2 should not be valid for wrong hash" $ do
        singleSigValid fb2 ss2 @?= False
    , testCase "single sig 3 should not be valid for wrong hash" $ do
        singleSigValid fb2 ss3 @?= False
    ]

  ------------------------------------------------------------------------------
  -- Multi sigs
  ------------------------------------------------------------------------------

  , testGroup "Multi Sig operations"
    [ testCase "multi sig 1 should be valid" $ do
        let msig1OK = MultiSig [ss1, ss2]
        multiSigValid mpk1 fb1 msig1OK @?= True

    , testCase "multi sig 2 should be valid" $ do
        let msig2OK = MultiSig [ss1, ss2, ss3]
        multiSigValid mpk1 fb1 msig2OK @?= True

    , testCase "multi sig 3 should be valid" $ do
        let msig3OK = MultiSig [ss2, ss3]
        multiSigValid mpk1 fb1 msig3OK @?= True

    , testCase "multi sig 4 should be invalid" $ do
        let msig4Err = MultiSig [] -- no sigs at all
        multiSigValid mpk1 fb1 msig4Err @?= False

    , testCase "multi sig 5 should be invalid" $ do
        let msig5Err = MultiSig [ss1] -- too few sigs
        multiSigValid mpk1 fb1 msig5Err @?= False

    , testCase "multi sig 6 should be valid" $ do
        let msig6OK = MultiSig [ss1, ss2]
        multiSigValid mpk2 fb1 msig6OK @?= True

    , testCase "multi sig 7 should be invalid" $ do
        let msig7Err = MultiSig [ss1, ss3] -- pk3 is not in mpk2
        multiSigValid mpk2 fb1 msig7Err @?= False

    , testCase "multi sig 8 should be invalid for repeating pk" $ do
        let msig8Err = MultiSig [ss1, ss1] -- repeated pk
        multiSigValid mpk2 fb1 msig8Err @?= False

    , testCase "multi sig 3 should be invalid for wrong hash" $ do
        let msig3OK = MultiSig [ss2, ss3]
        multiSigValid mpk1 fb2 msig3OK @?= False

    , testCase "multi sig 6 should be invalid for wrong hash" $ do
        let msig6OK = MultiSig [ss1, ss2]
        multiSigValid mpk2 fb2 msig6OK @?= False
    ]
  ]

------------------------------------------------------------------------------
-- Fingerprints
------------------------------------------------------------------------------

fingerprintSpec :: TestTree
fingerprintSpec = testGroup "Fingerprint Tests"
  [ testCase "multi sig 1 serialization should match" $ do
      mpk1 `shouldBeHex` "00033363a313e34cf6d3b9e0ce44aed5a54567c4302b873dd69ec7f37b9e83aabf6542fb07466d301ca2cc2eff2fd93a67eb1ebbec213e6532a04dc82be6a41329ae22b9524d37a16c945deec3455d92a1ebc5ac857174f5a0a8b376517a205dca730002"

  , testCase "multi sig 1 fingerprint should match" $ do
      computeHash mpk1 `shouldBeHex` "6f25872869654adb946b83b82490b2f38c001212e6815f86f41134ffd05c8327"

  , testCase "multi sig 2 fingerprint should match" $ do
      computeHash mpk2 `shouldBeHex` "1e974300f36903173a25402220e346503bc747e4549b608543939566f74ffe83"
  ]

daSchema0 :: DataHash
daSchema0 = computeHash @Bytes4 $ ofHex "deadbeef"

topicSchema0 :: DataHash
topicSchema0 = computeHash @Bytes4 $ ofHex "1ea7f00d"

committee0 :: Committee
committee0 = MultiSigPubKey ([pk1, pk2], UInt16 2)

daMetaData0 :: DaMetaData HashRef
daMetaData0 = DaMetaData daSchema0 (LiftRef (digestRef committee0))

timestamp1 :: POSIXTime
timestamp1 = 455155200000 -- June 4th 1989

msgMeta1 :: MessageMetaData HashRef
msgMeta1 = MessageMetaData pk1 timestamp1

msg1 :: BuiltinByteString
msg1 = stringToBuiltinByteString "Hello, World!"

msg1Hash :: DataHash
msg1Hash = computeHash msg1

msg2 :: BuiltinByteString
msg2 = stringToBuiltinByteString "Taxation is Theft"

msg3 :: BuiltinByteString
msg3 = stringToBuiltinByteString "Slava Drakonu"

msg3Hash :: DataHash
msg3Hash = computeHash msg3

daSpec :: TestTree
daSpec = testGroup "simple tests for SkyDA"
  let da0 = runIdentity $ initDa daSchema0 committee0 :: SkyDa HashRef
      (da1, Just topic0) = runIdentity $ insertTopic topicSchema0 da0
      (da2, Just msg1i) = runIdentity $ insertMessage pk1 timestamp1 msg1 topic0 da1
      (da3, _) = runIdentity $ insertMessage pk1 timestamp1 msg2 topic0 da2
      Just (msg1b, proof1) = runIdentity $ getSkyDataProof (topic0, msg1i) da3 :: Maybe (LiftRef HashRef BuiltinByteString, SkyDataProof Blake2b_256)
  in
  [ -- Create an empty SkyDA, check its digest
    testCase "hash of empty DA" $ do
      let topHash0 = computeHash da0
      topHash0 `shouldBeHex` "8fb3f562be2da84052ca81850060c549ac8b9914799885c8e1651405a3c38d19"

    -- Check that proofs come in empty
  , testCase "No proof for (0,0) in empty Da" $ do
      let maybeProof0 = runIdentity $ getSkyDataProof (fromInt 0, fromInt 0) da0 :: Maybe (LiftRef HashRef BuiltinByteString, SkyDataProof Blake2b_256)
      maybeProof0 `shouldBeHex` "00"

  , testCase "msg1 matches" $ do
      msg1b == LiftRef (digestRef msg1) @?= True

  , let l1d = (castDigest . getDigest . liftref $ msg1b) :: DataHash
        topHash1 = computeHash da3 :: DataHash
    in testCase "proof1 correct" $ do
      applySkyDataProof proof1 l1d == topHash1 @?= True
      (triePathHeight . pathTopicTriePath $ proof1) == 0 @?= True
      (triePathKey . pathTopicTriePath $ proof1) == topic0 @?= True
      (triePathHeight . pathMessageTriePath $ proof1) == 0 @?= True
      (triePathKey . pathMessageTriePath $ proof1) == msg1i @?= True

  , let (da10, Just topic1) = runIdentity $ insertTopic topicSchema0 da3
        (da11, Just msg3i) = runIdentity $ insertMessage pk1 timestamp1 msg3 topic1 da10
        Just (msg3b, _proof3) = runIdentity $ getSkyDataProof (topic1, msg3i) da11 :: Maybe (LiftRef HashRef BuiltinByteString, SkyDataProof Blake2b_256)
    in testCase "msg3 matches" $ do
      msg3b == LiftRef (digestRef msg3) @?= True

  , let (da10, Just topic1) = runIdentity $ insertTopic topicSchema0 da3
        (da11, Just msg3i) = runIdentity $ insertMessage pk1 timestamp1 msg3 topic1 da10
        Just (msg3b, proof3) = runIdentity $ getSkyDataProof (topic1, msg3i) da11 :: Maybe (LiftRef HashRef BuiltinByteString, SkyDataProof Blake2b_256)
        l3d = (castDigest . getDigest . liftref $ msg3b) :: DataHash
        topHash3 = computeHash da11 :: DataHash
    in testCase "proof3 correct" $ do
      applySkyDataProof proof3 l3d == topHash3 @?= True
      (triePathHeight . pathTopicTriePath $ proof3) == 0 @?= True
      (triePathKey . pathTopicTriePath $ proof3) == topic1 @?= True
      (triePathHeight . pathMessageTriePath $ proof3) == 0 @?= True
      (triePathKey . pathMessageTriePath $ proof3) == msg3i @?= True

  , let topHash1 = computeHash da3 :: DataHash
        (da10, Just topic1) = runIdentity $ insertTopic topicSchema0 da3
        (da11, Just _msg3i) = runIdentity $ insertMessage pk1 timestamp1 msg3 topic1 da10
        topHash3 = computeHash da11 :: DataHash
    in testCase "hashes differ" $ do
      topHash1 == topHash3 @?= False
      topHash1 `shouldBeHex` "f889a9fec14bad5dfd59bd560a6d7626f11f048a194f813a8b090745ed243253"
      topHash3 `shouldBeHex` "c0bd8a731290df2fe149240c8271e56182b9d211ab8b9e5ba4ea7073af9dbc8a"

  , let topHash1 = computeHash da3 :: DataHash
        topHash1Sig1 = SingleSig (pk1, signMessage sk1 topHash1)
    in testCase "topHash1Sig1 signature should be valid" $ do
      singleSigValid topHash1 topHash1Sig1 @?= True

  , let topHash1 = computeHash da3 :: DataHash
        topHash1Sig2 = SingleSig (pk2, signMessage sk2 topHash1)
    in testCase "topHash1Sig2 signature should be valid" $ do
      singleSigValid topHash1 topHash1Sig2 @?= True

  , let topHash1 = computeHash da3 :: DataHash
        topHash1Sig1 = SingleSig (pk1, signMessage sk1 topHash1)
        topHash1Sig2 = SingleSig (pk2, signMessage sk2 topHash1)
        topHash1Sig = MultiSig [topHash1Sig1, topHash1Sig2]
    in testCase "topHash1Sig multi-signature should be valid" $ do
      multiSigValid committee0 topHash1 topHash1Sig @?= True
  ]
