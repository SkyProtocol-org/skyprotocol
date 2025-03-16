{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TrieUtilsSpec (spec) where

import TrieUtils

import PlutusTx.Prelude
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Show
import PlutusTx.Utils

import Test.Hspec
import Test.QuickCheck hiding ((.&.))


instance
  (StaticLength len) =>
  Arbitrary (FixedLengthInteger len) where
  arbitrary = do
    let maxValue = (exponential 2 (staticLength @len)) - 1
    return . FixedLengthInteger $ chooseInteger (0, maxValue)
instance
  (StaticLength len) =>
  Arbitrary (FixedLengthByteString len) where
  arbitrary = do
    i <- arbitrary @(FixedLengthInteger len)
    return $ fromInt i
instance
  Arbitrary VariableLengthInteger where
  arbitrary = do
    let maxValue = (exponential 2 64) - 1
    return . VariableLengthInteger $ chooseInteger (0, maxValue)
instance
  Arbitrary VariableLengthByteString where
  arbitrary = do
    i <- arbitrary @VariableLengthInteger
    return $ fromInt i

testIntBitLogic :: (HasBitLogic a, HasFromInt a) => Maybe Integer -> Expectation
testIntBitLogic maybeMaxBits =
  describe "Checking BitLogic" $ do
    it "foo" $ do
      return true

spec :: Spec
spec = do
  describe "Utils" $ do
    it "multiplyByExponential" $ do
      multiplyByExponential 3 2 10 `shouldBe` 3072
      multiplyByExponential 2 3 5 `shouldBe` 486
    it "exponential" $ do
      exponential 7 4 `shouldBe` 2401
      exponential 4 7 `shouldBe` 16384
    it "toByte" $ do
      toByte 0 `shouldBe` Byte 0
      toByte 42 `shouldBe` Byte 42
      toByte 256 `shouldBe` Byte 256
      toByte 259 `shouldThrow` anyException
      toByte -10 `shouldThrow` anyException
    it "toUInt16" $ do
      toUInt16 0 `shouldBe` UInt16 0
      toUInt16 42 `shouldBe` UInt16 42
      toUInt16 256 `shouldBe` UInt16 256
      toUInt16 259 `shouldBe` UInt16 259
      toUInt16 -10 `shouldThrow` anyException
      toUInt16 65535 `shouldBe` UInt16 65535
      toUInt16 65536 `shouldThrow` anyException

      let i = bitLength :: Integer -> Integer
      let j = bitLength :: Byte32 -> Int
      i 0 `shouldBe` 0
      i 1 `shouldBe` 1
      i 2 `shouldBe` 2
      i 3 `shouldBe` 2
      i 4 `shouldBe` 3
      i 127 `shouldBe` 7
      i 128 `shouldBe` 8
      i 255 `shouldBe` 8
      i 256 `shouldBe` 9
      i 1000 `shouldBe` 10
      i (-1) `shouldBe` 0
      i (-2) `shouldBe` 1
      i (-3) `shouldBe` 2
      i (-4) `shouldBe` 2
      i (-5) `shouldBe` 3
      i (-128) `shouldBe` 7
      i (-129) `shouldBe` 8
      i (-256) `shouldBe` 8
      i (-257) `shouldBe` 9
      i (-1000) `shouldBe` 10
      j 0 `shouldBe` 0
      j 1 `shouldBe` 1
      j 2 `shouldBe` 2
      j 3 `shouldBe` 2
      j (-1) `shouldBe` 64

    it "QuickCheck property: integerLength" $
      property $
        \(n :: Word64) ->
          do
            let l = integerLength n
            l == 64 || n < 1 `shiftL` l `shouldBe` True
            n == 0 || 1 `shiftL` (l - 1) <= n `shouldBe` True
            let n' :: Integer = - (fromIntegral n) - 1
            let l' = integerLength n'
            l' == 64 || n' >= (-1) `shiftL` l' `shouldBe` True
            l' == 0 || n' < (-1) `shiftL` (l' - 1) `shouldBe` True
            l' `shouldBe` l

    it "lowestBitSet" $ do
      let l = lowestBitSet :: Int -> Int
      map l [0, 1, 2, 3, 4, 8, 100, -1, -2, -100] `shouldBe` [-1, 0, 1, 0, 2, 3, 2, 0, 1, 2]

    it "QuickCheck property: lowestBitSet" $
      property $
        \(n :: Word64) ->
          do
            let b = lowestBitSet n
            b == -1 `shouldBe` n == 0
            b == -1 || testBit n b `shouldBe` True
            fbLowestBitSet n `shouldBe` b
            b == -1 || n .&. lowBitsMask b == 0 `shouldBe` True
            let n' :: Integer = - fromIntegral n
            let b' = lowestBitSet n'
            b' `shouldBe` b

    it "lowestBitClear" $ do
      let l = lowestBitClear :: Int -> Int
      map l [0, 1, 2, 3, 4, 8, 100, -1, -2, -100] `shouldBe` [0, 1, 0, 2, 0, 0, 0, -1, 0, 0]
      let l' = lowestBitClear :: Word64 -> Int
      map l' [0, 1, 2, 3, 4, 8, 100, fromIntegral (-1), fromIntegral(-2), fromIntegral (-100)] `shouldBe`
        [0, 1, 0, 2, 0, 0, 0, -1, 0, 0]

    it "QuickCheck property: lowestBitClear" $
      property $
        \(n :: Word64) ->
          do
            let b = lowestBitClear n
            b == -1 `shouldBe` n == (fromIntegral (-1))
            b == -1 || not (testBit n b) `shouldBe` True
            fbLowestBitClear n `shouldBe` b
            b == -1 || n .&. lowBitsMask b == lowBitsMask b `shouldBe` True

    it "lowBitsMax" $ do
      let l = lowBitsMask :: Int -> Int
      map l [0, 1, 2, 3, 4, 7, 8, 16, 32, 63] `shouldBe`
        [0, 1, 3, 7, 15, 127, 255, 65535, 4294967295, 9223372036854775807]
      ((lowBitsMask (-1)) :: Word64) `shouldBe` (fromIntegral (-1))

    it "extractBitField" $ do
      let e (len, start, bits :: Int) = extractBitField len start bits
      e (3, 4, 37) `shouldBe` 2
      e (4, 2, 11) `shouldBe` 2

    it "QuickCheck property: extractBitField" $ property $
        \((len, start) :: (Word8, Word8)) -> do
          let l = fromIntegral len
          let s = fromIntegral start
          extractBitField l s (-1 :: Integer) `shouldBe` (lowBitsMask l)
