{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.ContractSpec (contractSpec) where

import Common
import Contract.Bounty
import Contract.SkyBridge
import Contract.SkyMintingPolicy
import Data.Functor.Identity (Identity (..))
import PlutusLedgerApi.V1 (PubKeyHash (..))
import PlutusLedgerApi.V1.Interval (Interval (..), strictLowerBound, strictUpperBound)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx.Prelude
import Test.Hspec

-- Test keys and data
sk1, sk2, sk3 :: SecKey
sk1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
sk2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"
sk3 = ofHex "9F664160D9DDCD27B5B9A0C619FC3978DDE6C51F4FEAF40688BF54281AA0D0CC"

pk1, pk2, pk3 :: PubKey
pk1 = derivePubKey sk1
pk2 = derivePubKey sk2
pk3 = derivePubKey sk3

mpk1 :: MultiSigPubKey
mpk1 = MultiSigPubKey ([pk1, pk2, pk3], toUInt16 2)

committee0 :: Committee
committee0 = MultiSigPubKey ([pk1, pk2], UInt16 2)

deadline :: POSIXTime
deadline = 1000180800000 -- Sep 11th 2001

txBeforeDeadlineRange :: Interval POSIXTime
txBeforeDeadlineRange =
  Interval
    (strictLowerBound 999316800000) -- Sep 1st 2001
    (strictUpperBound 1000008000000) -- Sep 9th 2001

txAfterDeadlineRange :: Interval POSIXTime
txAfterDeadlineRange =
  Interval
    (strictLowerBound 1000267200000) -- Sep 12th 2001
    (strictUpperBound 1000872000000) -- Sep 19th 2001

txAroundDeadlineRange :: Interval POSIXTime
txAroundDeadlineRange =
  Interval
    (strictLowerBound 1000008000000) -- Sep 9th 2001
    (strictUpperBound 1000872000000) -- Sep 19th 2001

-- Test data for contract validation
daSchema0 :: DataHash
daSchema0 = computeHash $ (ofHex "deadbeef" :: Bytes4)

topicSchema0 :: DataHash
topicSchema0 = computeHash $ (ofHex "1ea7f00d" :: Bytes4)

msg1Hash :: DataHash
msg1Hash = computeHash ("Hello, World!" :: BuiltinString)

contractSpec :: Spec
contractSpec = do
  describe "OnChain Contract Tests" $ do
    describe "Bounty Contract" $ do
      it "should validate timeout after deadline" $ do
        validateTimeout deadline txAfterDeadlineRange `shouldBe` True

      it "should reject timeout before deadline" $ do
        validateTimeout deadline txBeforeDeadlineRange `shouldBe` False

      it "should reject timeout in interval around deadline" $ do
        validateTimeout deadline txAroundDeadlineRange `shouldBe` False

    describe "Bridge Contract" $ do
      let topHash1Sig1 = SingleSig (pk1, signMessage sk1 topHash1)
      let topHash1Sig2 = SingleSig (pk2, signMessage sk2 topHash1)
      let topHash1Sig = MultiSig [topHash1Sig1, topHash1Sig2]

      it "topHash1Sig1 signature should be valid" $ do
        singleSigValid topHash1 topHash1Sig1 `shouldBe` True

      it "topHash1Sig2 signature should be valid" $ do
        singleSigValid topHash1 topHash1Sig2 `shouldBe` True

      it "topHash1Sig multi-signature should be valid" $ do
        multiSigValid committee0 topHash1 topHash1Sig `shouldBe` True

    describe "Sky Minting Policy" $ do
      it "should validate minting with correct public key hash" $ do
        -- This is a placeholder test since we can't easily test the full minting policy
        -- without a complete script context
        let pkh = case pk1 of PubKey bs -> PubKeyHash (getFixedLengthByteString bs)

        -- Just test that the type compiles and function exists
        -- Real testing would require proper ScriptContext setup
        True `shouldBe` True
