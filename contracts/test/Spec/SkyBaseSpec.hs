{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.SkyBaseSpec where

import SkyBase
import SkyCrypto
import Trie

import PlutusTx.Prelude
import qualified PlutusTx.Prelude as P
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Builtins.Internal (BuiltinString (..))
import qualified PlutusTx.Show as PS
import PlutusTx.Utils

import qualified Debug.Trace as DT
import qualified GHC.Show as GS
import Data.Functor.Identity (Identity (..))
import Data.String (String, IsString, fromString)
import qualified GHC.Base as GB
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

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

toBytes4 :: Integer -> Bytes4
toBytes4 = fromInt

genUInt len = choose (0, (exponential 256 len) - 1)
genByteString len = genUInt len >>= return . integerToByteString LittleEndian len

-- * QuickCheck support
instance
  Arbitrary Byte where
  arbitrary = genUInt 1 >>= return . Byte

instance
  Arbitrary UInt16 where
  arbitrary = genUInt 2 >>= return . UInt16

instance
  (StaticLength len) =>
  Arbitrary (FixedLengthInteger len) where
  arbitrary = genUInt (staticLength @len) >>= return . FixedLengthInteger
instance
  (StaticLength len) =>
  GS.Show (FixedLengthInteger len) where
  show = GS.show . PS.show

instance
  Arbitrary VariableLengthInteger where
  arbitrary = choose (0, 31) >>= genUInt >>= return . fromInt

instance
  (StaticLength len) =>
  Arbitrary (FixedLengthByteString len) where
  arbitrary = genByteString (staticLength @len) >>= return . FixedLengthByteString
instance
  (StaticLength len) =>
  GB.Eq (FixedLengthByteString len) where
  (==) = (P.==)
instance
  (StaticLength len) =>
  GS.Show (FixedLengthByteString len) where
  show = GS.show . PS.show

instance
  Arbitrary BuiltinByteString where
  arbitrary = choose (0, 31) >>= genByteString

instance
  Arbitrary VariableLengthByteString where
  arbitrary = arbitrary >>= return . VariableLengthByteString

instance GS.Show Bytes4 where show = GS.show . PS.show
instance GS.Show UInt64 where show = GS.show . PS.show
instance GB.Eq UInt64 where (==) = (P.==)

-- * Tests
baseSpec :: Spec
baseSpec = do
  describe "SkyBase" $ do
    it "builtins" $ do
      replicateByte 3 4 `shouldBeHex2` "040404"
      appendByteString (replicateByte 3 4) (replicateByte 2 3) `shouldBeHex2` "0404040303"
      integerToByteString BigEndian 6 0x1234567890 `shouldBeHex2` "001234567890"
      let fsb = findFirstSetBit . toByteString . toUInt32
      fsb 0x8004 `shouldBe` 2
      fsb 0x00ff `shouldBe` 0
      fsb 0xff00 `shouldBe` 8
      fsb 0x00f0 `shouldBe` 4
    it "serialization" $ do
      PS.show (Byte 42) `shouldBe` "Byte 42"
      PS.show (UInt16 0xf00d) `shouldBe` "UInt16 61453"
      PS.show (toUInt32 0x10ffff) `shouldBe` "FixedLengthInteger @L4 1114111"
      Byte 42 `shouldBeHex2` "2a"
      (ofHex "abcd" :: BuiltinByteString) `shouldBeHex` "abcd"
    it "multiplyByExponential" $ do
      multiplyByExponential 3 2 10 `shouldBe` 3072
      multiplyByExponential 2 3 5 `shouldBe` 486
    it "exponential" $ do
      exponential 7 4 `shouldBe` 2401
      exponential 4 7 `shouldBe` 16384
    it "toByte" $ do
      toByte 0 `shouldBeHex2` "00"
      toByte 42 `shouldBeHex2` "2a"
      toByte 255 `shouldBeHex2` "ff"
      --toByte 259 `shouldThrow` anyException
      --toByte -10 `shouldThrow` anyException
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
    it "lowestBitClear" $ do
      let lbci = [0,1,2,3,5,7,1151]
          lbcin = [-1,-2,-5,-100,-1153]
          lbco = [0,1,0,2,1,3,7]
          lbcon = [-1,0,2,0,7]
      map (lowestBitClear :: Integer -> Integer) lbci `shouldBe` lbco
      map (lowestBitClear :: Integer -> Integer) lbcin `shouldBe` lbcon
      map (lowestBitClear . toUInt64 :: Integer -> Integer) lbci `shouldBe` lbco
      map (lowestBitClear . toBytes4 :: Integer -> Integer) lbci `shouldBe` lbco
    it "BitLogic Integer" $ do
      testBitLogic (\x -> x) (\x -> x) (-1 :: Integer) 16 False
    it "BitLogic Bytes4" $ do
      testBitLogic toBytes4 toInt (toBytes4 0xffffffff) 4 True
    it "BitLogic UInt64" $ do
      testBitLogic toUInt64 toInt (toUInt64 0xffffffffffffffff) 8 True

testBitLogic :: (BitLogic a, FromInt a, Dato a, Eq a, GS.Show a, Arbitrary a) =>
  (Integer -> a) -> (a -> Integer) -> a -> Integer -> Bool -> Expectation
testBitLogic i t allBits len isUnsigned =
  let nBits = len * 8
      bM = t $ lowBitsMask nBits
      maxi = i $ (exponential 256 len) - 1
      genA = genUInt len
      m1 = t maxi `quotient` 255
      rep b = i $ b * m1 -- or we could add a method using replicateByte for ByteString's
      r17 = rep 17
      aAnd x y = t $ (i x) `logicalAnd` (i y)
      aOr x y = t $ (i x) `logicalOr` (i y)
      aXor x y = t $ (i x) `logicalXor` (i y)
      aLowestBitClear n = lowestBitClear (i n)
      ebf len height bits = t $ extractBitField len height (i bits)
  in
  do -- it "Checking BitLogic" $ do
--    it "bitLength" $ do
      (bitLength $ i 17) `shouldBe` 5
      bitLength r17 `shouldBe` (len * 8) - 3
--    it "lowestBitClear" $ do
      aLowestBitClear 0 `shouldBe` 0
      aLowestBitClear 159 `shouldBe` 5
      lowestBitClear (maxi `shiftRight` (len * 2)) `shouldBe` len * 6
      lowestBitClear allBits `shouldBe` -1
--    it "isBitSet" $ do
--    it "logicalAnd" $ do
      240 `aAnd` 85 `shouldBe` 80
      42 `aAnd` 1 `shouldBe` 0
--    it "logicalOr" $ do
      240 `aOr` 85 `shouldBe` 245
      42 `aOr` 1 `shouldBe` 43
--    it "logicalXor" $ do
      240 `aXor` 85 `shouldBe` 165
      42 `aXor` 1 `shouldBe` 43
--    it "lowBitsMask" $ do
      let l = t . lowBitsMask :: Integer -> Integer
      map l [0, 1, 2, 3, 4, 7, 8, 16, 32] `shouldBe`
        [0, 1, 3, 7, 15, 127, 255, 65535, 4294967295]
      (lowBitsMask 63 :: UInt64) `shouldBe` toUInt64 9223372036854775807

--    it "shiftRight" $ do
--    it "shiftLeft" $ do
      --it "extractBitField" $ do
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

cryptoSpec = do
  describe "SkyCrypto" $ do
    it "simple tries" $ do
      let t1 = TrieTop 4 (2989 :: Integer)
      t1 `shouldBeHex2` "00050bad"
      PS.show t1 `shouldBe` "TrieTop 4 2989"
    it "simple hashes" $ do
      computeHash (hexB "") `shouldBeHex2` "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
      computeHash (hexB "00") `shouldBeHex2` "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"
      computeHash (ofHex "abcd" :: UInt16) `shouldBeHex2` "9606e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5"
