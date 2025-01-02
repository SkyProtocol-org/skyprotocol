{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TrieKeySpec (spec) where

import Data.Bits
import Data.Internal.Trie
import Data.WideWord (Word256)
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

instance TrieKey Word256 where
  type TrieHeight Word256 = Word8

instance Arbitrary Word256 where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
  describe "TrieKey typeclass" $ do
    it "mask should discard bits not in the mask" $ do
      mask @Word256 0b1101 0b0 `shouldBe` 0b0 -- zero mask should return zero
      mask @Word256 0b1101 0b0100 `shouldBe` 0b1100
      mask @Word256 0b1101 0b0010 `shouldBe` 0b1100
      mask @Word256 0b1101 0b0001 `shouldBe` 0b1101

    it "commonBranchingBit should find the first bit on which prefixes disagree" $ do
      commonBranchingBit @Word256 0b1100 0b1000 `shouldBe` 0b0100
      commonBranchingBit @Word256 0b1100 0b1110 `shouldBe` 0b0010

    it "matchPrefix should mask the key using supplied branching bit and compare to prefix" $ do
      matchPrefix @Word256 0b1101 0b1100 0b0100 `shouldBe` True
      matchPrefix @Word256 0b1101 0b1000 0b0100 `shouldBe` False

    it "heightToBBit should convert height Word256 to the branching bit" $ do
      heightToBBit @Word256 0 `shouldBe` setBit 0 255
      heightToBBit @Word256 1 `shouldBe` setBit 0 254

    it "bBitToHeight should convert branching bit Word256 to height" $ do
      bBitToHeight @Word256 (setBit 0 255) `shouldBe` 0
      bBitToHeight @Word256 (setBit 0 254) `shouldBe` 1

    it "zeroBit should test whether the desired bit is zero" $ do
      zeroBit @Word256 0b1101 (heightToBBit @Word256 1) `shouldBe` True
      zeroBit @Word256 0b1101 (heightToBBit @Word256 2) `shouldBe` False

    it "QuickCheck property: heightToBBit and bBitToHeight should be inverses" $
      property $
        \(h :: Word8) ->
          bBitToHeight @Word256 (heightToBBit @Word256 h) == h
