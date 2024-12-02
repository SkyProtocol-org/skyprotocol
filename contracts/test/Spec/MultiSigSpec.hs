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

sig1 :: BuiltinByteString
sig1 = hex "184E401E7EA7C7E2F9B4186DEDF953437F81BD2664D1FBDE525264A4D08BFD79D81877376F1E63CE64DF46C5F1FD93CDF3B05B8B6076A6ADC05F36C81F62A500"

ss1 :: SingleSig
ss1 = SingleSig pk1 sig1

spec :: Spec
spec = do
  describe "Single Sig operations" $ do
    it "single sig should be valid" $ do
      (singleSigValid dh1 ss1) `shouldBe` True
    it "single sig should not be valid for wrong hash" $ do
      (singleSigValid dh2 ss1) `shouldBe` False
