{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.TypesSpec where

import Common.Types
import Common.Crypto

import PlutusTx.Prelude
import qualified PlutusTx.Prelude as P
import PlutusTx.Builtins
import qualified PlutusTx.Show as PS
import qualified GHC.Base as GB
import qualified GHC.Show as GS
import Data.String (String)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Control.Exception

-- * Helpers
shouldBeHex :: (ToByteString a) => a -> GB.String -> Expectation
a `shouldBeHex` h =
  hexOf a `shouldBe` h

shouldBeHex2 :: (Eq a, ToByteString a, FromByteString a) => a -> String -> Expectation
a `shouldBeHex2` h = do
  a `shouldBeHex` h
  let b = ofHex h
  a == b `shouldBe` True
  hexOf b `shouldBe` h

genUInt :: Integer -> Gen Integer
genUInt len = choose (0, exponential 256 len - 1)

genByteString :: Integer -> Gen BuiltinByteString
genByteString n = genUInt n >>= return . integerToByteString LittleEndian n

toBytes4 :: Integer -> Bytes4
toBytes4 = fromInt

toBytes8 :: Integer -> Bytes8
toBytes8 = fromInt

-- * QuickCheck support
instance Arbitrary Byte where
  arbitrary = genUInt 1 >>= return . Byte

instance Arbitrary UInt16 where
  arbitrary = genUInt 2 >>= return . UInt16

instance (StaticLength len) =>
  Arbitrary (FixedLengthInteger len) where
  arbitrary = genUInt (staticLength $ Proxy @len) >>= return . FixedLengthInteger

instance
  (StaticLength len) =>
  GS.Show (FixedLengthInteger len) where
  show = GS.show . PS.show

instance (StaticLength len) => Arbitrary (FixedLengthByteString len) where
  arbitrary = genByteString (staticLength $ Proxy @len) >>= return . FixedLengthByteString

instance (HashFunction hf) => GS.Show (Digest hf a)
  where
  show = GS.show . PS.show

instance Arbitrary BuiltinByteString where
  arbitrary = choose (0, 31) >>= genByteString

instance GB.Eq UInt64 where (==) = (P.==)

instance Exception String where

-- * Tests
typesSpec :: Spec
typesSpec = do
  describe "SkyBase" $ do
    it "builtins" $ do
      replicateByte 3 4 `shouldBeHex2` "040404"
      appendByteString (replicateByte 3 4) (replicateByte 2 3) `shouldBeHex2` "0404040303"
      integerToByteString BigEndian 6 0x1234567890 `shouldBeHex2` "001234567890"
      let fsb = findFirstSetBit . toByteString . toUInt32
      -- NOPE: logInfo @String "foo" `shouldBe` True
      fsb 0x8004 `shouldBe` 2
      fsb 0x00ff `shouldBe` 0
      fsb 0xff00 `shouldBe` 8
      fsb 0x00f0 `shouldBe` 4
      fsb 0x0000 `shouldBe` -1
      fsb 0xffffffff `shouldBe` 0
      fsb 0x80000000 `shouldBe` 31
    it "exception handling" $ do
      -- Attempt to call the function and catch any exceptions
      --result <- try (evaluate (GE.error "FOO")) :: GB.IO (Either ErrorCall Integer)
      result <- try (evaluate (toInt . toByte $ 257)) :: GB.IO (Either ErrorCall Integer)
      case result of
        Left (ErrorCall x)  -> do
          -- putStrLn $ "The exception is: " GB.++ x
          trace "FOOOOOO" $ -- dropped on the ground! :-(
            x `shouldBe` "PlutusTx.Builtins.Internal.error" -- unhappily uninformative
        Right val -> do
          -- putStrLn $ "The result is: " GB.++ (GS.show $ PS.show val)
          val `shouldBe` 257
          True `shouldBe` False -- wrong to reach this

    it "serialization 1" $ PS.show (Byte 42) `shouldBe` "42"
    it "serialization 2" $ do
      PS.show (UInt16 0xf00d) `shouldBe` "61453"
      PS.show (toUInt16 0xbad) `shouldBe` "2989"
      PS.show (maybeFromInt 0x11111 :: Maybe UInt16) `shouldBe` "Nothing"
    it "serialization 3" $ PS.show (toUInt32 0x10ffff) `shouldBe` "FixedLengthInteger @L4 1114111"
    it "serialization 4" $ PS.show (fromInt 0x5678901234567890 :: Bytes8) `shouldBe` "5678901234567890"
    it "serialization 5" $ do
      let unicodeMax :: Integer
          unicodeMax = 0x10ffff
      unicodeMax == 1114111 `shouldBe` True
      PS.show unicodeMax `shouldBe` "1114111"
      (unicodeMax,33 :: Integer) `shouldBeHex2` "000310ffff21"
    it "serialization 7" $
      -- | Plutus doesn't quote not readably output its build
      PS.show (5::Integer, "23"::BuiltinString, hexB "0badf00d") `shouldBe` "(5,\"23\",0badf00d)"
    it "serialization 8" $ do
      Byte 42 `shouldBeHex2` "2a"
      (ofHex "abcd" :: BuiltinByteString) `shouldBeHex` "abcd"
    it "multiplyByExponential" $ do
      multiplyByExponential 3 2 10 `shouldBe` 3072
      multiplyByExponential 2 3 5 `shouldBe` 486
    it "exponential" $ do
      exponential 7 4 `shouldBe` 2401
      exponential 4 7 `shouldBe` 16384
      exponential 2 63 `shouldBe` 9223372036854775808
      exponential 3 42 `shouldBe` 109418989131512359209
      bitLength (exponential 3 42) `shouldBe` 67
      -- a Mersenne number, large enough to require Bignums, yet not too large either (66 bytes)
      let m521 = exponential 2 521 - 1
      PS.show m521 `shouldBe` "6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151"
      bitLength m521 `shouldBe` 521
      map (exponential 2) [0..16] `shouldBe`
        [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536]
    it "toByte" $ do
      toByte 0 `shouldBeHex2` "00"
      toByte 42 `shouldBeHex2` "2a"
      toByte 255 `shouldBeHex2` "ff"
      --toByte 259 `shouldThrow` anyException
      --toByte -10 `shouldThrow` anyException
      -- 000302020100016101000162030000000000000000000163
    it "toUInt16" $ do
      toUInt16 0 `shouldBeHex2` "0000"
      toUInt16 42 `shouldBeHex2` "002a"
      toUInt16 256 `shouldBeHex2` "0100"
      toUInt16 259 `shouldBeHex2` "0103"
      --toUInt16 -10 `shouldThrow` anyException
      toUInt16 65535 `shouldBeHex2` "ffff"
      --toUInt16 65536 `shouldThrow` anyException
    it "equalizeByteStringLength" $ do
      let e x y = (hexOf ex :: BuiltinString, hexOf ey :: BuiltinString) where (ex, ey) = equalizeByteStringLength (ofHex x) (ofHex y)
      e "abcd" "ef01" `shouldBe` ("abcd", "ef01")
      e "abcd" "" `shouldBe` ("abcd", "0000")
      e "ab" "ef01" `shouldBe` ("00ab", "ef01")
      e "1234567890" "00" `shouldBe` ("1234567890", "0000000000")
    it "bitLength" $ do
      map bitLength2 [0, 1, 2, 3] `shouldBe` [0, 1, 2, 2]
      map bitLength4 [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] `shouldBe` [0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4]
      map bitLength8 [0, 3, 4, 15, 18, 42, 126, 255] `shouldBe` [0, 2, 3, 4, 5, 6, 7, 8]
      let nums32 = [0,1,2,3,4,15,18,42,126,128,255,256,1000,0xffff,0x12345,0x1badf00d,0xdeadbeef,0xffffffff]
          bls32 = [0,1,2,2,3,4,5,6,7,8,8,9,10,16,17,29,32,32]
          bli = bitLength :: Integer -> Integer
      map bitLength nums32 `shouldBe` bls32
      map (bitLength . toUInt32) nums32 `shouldBe` bls32
      map (bitLength . toBytes4) nums32 `shouldBe` bls32
      map bli [-1,-2,-3,-4,-5,-128,-129,-256,-257,-1000] `shouldBe` [0,1,2,2,3,7,8,8,9,10]
      bli 1000000000000066600000000000001 `shouldBe` 100 -- Belphegor's Prime
    let lbci = [0,1,2,3,5,7,1151]
        lbco = [0,1,0,2,1,3,7]
        lbcin = [-1,-2,-5,-100,-1153]
        lbcon = [-1,0,2,0,7]
        lbbyte = [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,7,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,8]
    it "lowestBitClear 1.2" $ do
      map (lowestBitClear :: Integer -> Integer) lbci `shouldBe` lbco
    it "lowestBitClear 1.4" $ do
      map (lowestBitClear :: Integer -> Integer) lbcin `shouldBe` lbcon
    it "lowestBitClear 1.6" $ do
      map (lowestBitClear . toUInt64 :: Integer -> Integer) lbci `shouldBe` lbco
    it "lowestBitClear 1.8" $ do
      map (lowestBitClear . toBytes4 :: Integer -> Integer) lbci `shouldBe` lbco
    it "lowestBitClear 2" $ do
      map (lowestBitClear . toByte :: Integer -> Integer) [0..255] `shouldBe` lbbyte
    it "lowestBitClear 3" $ do
      map (lowestBitClear @(FixedLengthByteString L1) . fromInt :: Integer -> Integer) [0..255] `shouldBe` lbbyte
{-      map (\(n :: Integer) -> (n,
                               exponential 2 0,
                               n `quotient` 2,
                               lowestBitClear n,
                               isBitSet 0 n))
          [0] `shouldBe` [] -}
    testBitLogic "Integer" fromInt toInt (-1 :: Integer) 16 False
    testBitLogic "Bytes4" fromInt toInt (toBytes4 0xffffffff) 4 True
    testBitLogic "UInt64" fromInt toInt (toUInt64 0xffffffffffffffff) 8 True
    testBitLogic "Bytes8" fromInt toInt (toBytes8 0xffffffffffffffff) 8 True
    it "shiftLeftWithBits" $ do
      (toBytes8 128 `shiftLeft` 56) `shouldBeHex2` "8000000000000000"
      PS.show (shiftLeftWithBits (toBytes8 1) 63 (fromInt 0)) `shouldBe` "8000000000000000"

testBitLogic :: (BitLogic a, FromInt a, Dato a, Eq a, GS.Show a, Arbitrary a) =>
  String -> (Integer -> a) -> (a -> Integer) -> a -> Integer -> Bool -> Spec
testBitLogic typ i t allBits len isUnsigned =
  let nBits = len * 8
      -- bM = t $ lowBitsMask nBits
      maxi = i $ exponential 256 len - 1
      -- genA = genUInt len
      m1 = t maxi `quotient` 255
      rep b = i $ b * m1 -- or we could add a method using replicateByte for ByteString's
      r17 = rep 17
      aAnd x y = t $ i x `logicalAnd` i y
      aOr x y = t $ i x `logicalOr` i y
      aXor x y = t $ i x `logicalXor` i y
      aLowestBitClear n = lowestBitClear (i n)
      ebf l height bits = t $ extractBitField l height (i bits)
      itt x = it $ typ GB.++ " " GB.++ x
  in
  do -- it "Checking BitLogic" $ do
    itt "bitLength" $ do
      bitLength (i 17) `shouldBe` 5
      bitLength r17 `shouldBe` (len * 8) - 3
    itt "lowestBitClear" $ do
      aLowestBitClear 0 `shouldBe` 0
      aLowestBitClear 159 `shouldBe` 5
      lowestBitClear (maxi `shiftRight` (len * 2)) `shouldBe` len * 6
      lowestBitClear allBits `shouldBe` if isUnsigned then nBits else -1
    itt "isBitSet" $ do
      isBitSet 5 (i 0) `shouldBe` False
      isBitSet 0 (i 5) `shouldBe` True
      isBitSet 2 (i 7) `shouldBe` True
      isBitSet 7 (i 2) `shouldBe` False
    itt "logicalAnd" $ do
      240 `aAnd` 85 `shouldBe` 80
      42 `aAnd` 1 `shouldBe` 0
    itt "logicalOr" $ do
      240 `aOr` 85 `shouldBe` 245
      42 `aOr` 1 `shouldBe` 43
    itt "logicalXor" $ do
      240 `aXor` 85 `shouldBe` 165
      42 `aXor` 1 `shouldBe` 43
--    it "lowBitsMask" $ do
      let l = t . lowBitsMask :: Integer -> Integer
      map l [0, 1, 2, 3, 4, 7, 8, 16, 32] `shouldBe`
        [0, 1, 3, 7, 15, 127, 255, 65535, 4294967295]
      (lowBitsMask 63 :: UInt64) `shouldBe` toUInt64 9223372036854775807

--    it "shiftRight" $ do
--    it "shiftLeft" $ do
    itt "extractBitField" $ do
      ebf 3 4 37 `shouldBe` 2
      ebf 4 2 11 `shouldBe` 2
      --it "QuickCheck property: lowBitsMask and lowestBitClear" $
{- XXX debug that with Ya
      property $
        \ (n :: a) -> do
          let b = lowestBitClear n
          -1 <= b && b <= nBits `shouldBe` True
          0 <= b || (n == i 0 && not isUnsigned) `shouldBe` True
          b < nBits && isBitSet n b `shouldBe` False
          n `logicalOr` (allBits `logicalXor` lowBitsMask b) == allBits `shouldBe` True -}
