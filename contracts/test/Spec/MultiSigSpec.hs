module Spec.MultiSigSpec (spec) where

import Test.Hspec
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import Text.Hex (Text, ByteString, decodeHex)
import Data.Text (pack)
import Data.Maybe (fromJust)
import SkyBridgeContract

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

hex :: [Char] -> BuiltinByteString
hex s = fromJust (hexStringToBuiltinByteString (pack s))

-- Keys generated via https://cyphr.me/ed25519_tool/ed.html

dh1 :: DataHash
dh1 = DataHash $ hex "CAFE"

dh2 :: DataHash
dh2 = DataHash $ hex "BABE"

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

spec :: Spec
spec = do
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
