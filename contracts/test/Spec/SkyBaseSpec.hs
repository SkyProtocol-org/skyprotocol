{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.SkyBaseSpec where

import SkyBase
import SkyCrypto
import Trie

import PlutusTx.Prelude
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Builtins.Internal (BuiltinString (..))
import PlutusTx.Show
import PlutusTx.Utils

import qualified Debug.Trace as DT
import Data.Functor.Identity (Identity (..))
import Data.String (String, IsString, fromString)
import GHC.Base (String)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

-- * Helpers

shouldBeHex :: ToByteString a => a -> String -> Expectation
a `shouldBeHex` h = hexOf a `shouldBe` h

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
  Arbitrary VariableLengthInteger where
  arbitrary = choose (0, 31) >>= genUInt >>= return . fromInt

instance
  (StaticLength len) =>
  Arbitrary (FixedLengthByteString len) where
  arbitrary = genByteString (staticLength @len) >>= return . FixedLengthByteString

instance
  Arbitrary BuiltinByteString where
  arbitrary = choose (0, 31) >>= genByteString

instance
  Arbitrary VariableLengthByteString where
  arbitrary = arbitrary >>= return . VariableLengthByteString

spec :: Spec
spec = do
  describe "SkyBase" $ do
    it "builtins" $ do
      trace' (fromString "bar" :: BuiltinString) $
        replicateByte 3 4 `shouldBeHex` "040404"
      appendByteString (replicateByte 3 4) (replicateByte 2 3) `shouldBeHex` "0404040303"
      integerToByteString BigEndian 6 0x1234567890 `shouldBeHex` "001234567890"
      let fsb = findFirstSetBit . toByteString . toUInt32
      fsb 0x8004 `shouldBe` 2
      fsb 0x00ff `shouldBe` 0
      fsb 0xff00 `shouldBe` 8
      fsb 0x00f0 `shouldBe` 4
      Byte 42 `shouldBeHex` "2a"
      (ofHex "abcd" :: BuiltinByteString) `shouldBeHex` "abcd"
    it "multiplyByExponential" $ do
      multiplyByExponential 3 2 10 `shouldBe` 3072
      multiplyByExponential 2 3 5 `shouldBe` 486
    it "exponential" $ do
      exponential 7 4 `shouldBe` 2401
      exponential 4 7 `shouldBe` 16384
    it "toByte" $ do
      toByte 0 `shouldBeHex` "00"
      toByte 42 `shouldBeHex` "2a"
      toByte 255 `shouldBeHex` "ff"
      --toByte 259 `shouldThrow` anyException
      --toByte -10 `shouldThrow` anyException
    it "toUInt16" $ do
      toUInt16 0 `shouldBeHex` "0000"
      toUInt16 42 `shouldBeHex` "002a"
      toUInt16 256 `shouldBeHex` "0100"
      toUInt16 259 `shouldBeHex` "0103"
      --toUInt16 -10 `shouldThrow` anyException
      toUInt16 65535 `shouldBeHex` "ffff"
      --toUInt16 65536 `shouldThrow` anyException
    it "equalizeByteStringLength" $ do
      let e x y = (hexOf ex, hexOf ey) where (ex, ey) = equalizeByteStringLength (ofHex x) (ofHex y)
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

{-
    it "QuickCheck property: lowestBitSet" $
      property $
        \(n :: UInt64) ->
          do
            let b = lowestBitSet n
            b == -1 `shouldBe` n == 0
            b == -1 || testBit n b `shouldBe` True
            fbLowestBitSet n `shouldBe` b
            b == -1 || n `logicalAnd` lowBitsMask b == 0 `shouldBe` True
            let n' :: Integer = - toInt n
            let b' = lowestBitSet n'
            b' `shouldBe` b

    it "QuickCheck property: lowestBitClear" $
      property $
        \(n :: UInt64) ->
          do
            let b = lowestBitClear n
            b == -1 `shouldBe` n == (toInt (-1))
            b == -1 || not (testBit n b) `shouldBe` True
            fbLowestBitClear n `shouldBe` b
            b == -1 || n `logicalAnd` lowBitsMask b == lowBitsMask b `shouldBe` True

    it "lowBitsMax" $ do
      let l = lowBitsMask :: Integer -> Integer
      map l [0, 1, 2, 3, 4, 7, 8, 16, 32, 63] `shouldBe`
        [0, 1, 3, 7, 15, 127, 255, 65535, 4294967295, 9223372036854775807]
      ((lowBitsMask (-1)) :: UInt64) `shouldBe` (toInt (-1))

    it "extractBitField" $ do
      let e (len, start, bits :: Integer) = extractBitField len start bits
      e (3, 4, 37) `shouldBe` 2
      e (4, 2, 11) `shouldBe` 2

    it "QuickCheck property: extractBitField" $ property $
        \((len, start) :: (Byte, Byte)) -> do
          let l = toInt len
          let s = toInt start
          extractBitField l s (-1 :: Integer) `shouldBe` (lowBitsMask l)
-}

{-
testBitLogic :: (BitLogic a, FromInt a) => a -> Integer -> Expectation
testBitLogic allBits len =
  do -- it "Checking BitLogic" $ do
    let nBits = len * 8
        i = fromInt :: Integer -> a
        bM = lowBitsMask nBits :: a
        maxi = i $ (exponential 256 len) - 1
        genA = genUInt len
        m1 = maxi `quotient` 255
        rep b = i $ b * m1 -- or we could add a method using replicateByte for ByteString's
        r17 = rep 17
    it "bitLength" $ do
      (bitLength $ i 17) `shouldBe` 5
      bitLength r17 `shouldBe` (len * 8) - 3
    it "lowestBitClear" $ do
      lowestBitClear i 0 `shouldBe` 0
      lowestBitClear (i 159) `shouldBe` 5
      lowestBitClear allBits `shouldBe` -1
      lowestBitClear (maxi `shiftRight` (len * 2)) `shouldBe` len * 6
--    it "isBitSet" $ do
    it "logicalAnd" $ do
      i 240 `logicalAnd` i 85  `shouldBe` i 80
      i 42 `logicalAnd` i 1 `shouldBe` i 0
    it "logicalOr" $ do
      i 240 `logicalOr` i 85  `shouldBe` i 245
      i 42 `logicalOr` i 1 `shouldBe` i 43
    it "logicalXor" $ do
      i 240 `logicalXor` i 85 `shouldBe` i 165
      i 42 `logicalXor` i 1 `shouldBe` i 43
--    it "lowBitsMask" $ do
--    it "shiftRight" $ do
--    it "shiftLeft" $ do
-}

