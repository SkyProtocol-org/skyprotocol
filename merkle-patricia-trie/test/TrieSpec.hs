{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TrieSpec (spec) where

import Data.Trie qualified as Trie
import Data.WideWord (Word256)
import Data.Word (Word, Word8)
import Test.Hspec
import Test.QuickCheck

instance Trie.TrieKey Word256 where
  type TrieHeight Word256 = Word8

spec :: Spec
spec = do
  describe "Data.Trie" $ do
    it "lookup should return Nothing for an empty trie" $ do
      (Trie.lookup @Word256 @_ 1 Trie.empty :: Maybe Word256) `shouldBe` Nothing

    it "lookup should return the value for a key in a singleton trie" $ do
      let t = Trie.singleton @Word256 @_ 1 "value"
      Trie.lookup 1 t `shouldBe` Just "value"

    it "lookup should return Nothing for a key not in the trie" $ do
      let t = Trie.singleton @Word256 @_ 1 "value"
      Trie.lookup 2 t `shouldBe` Nothing

    it "insert should add a key-value pair to an empty trie" $ do
      let t = Trie.insert @Word256 @_ 1 "value" Trie.empty
      Trie.lookup 1 t `shouldBe` Just "value"

    it "insertWith should resolve conflicts using the provided function" $ do
      let t = Trie.insertWith @Word256 @_ (++) 1 "value1" $ Trie.insert 1 "value2" Trie.empty
      Trie.lookup 1 t `shouldBe` Just "value1value2"

    it "insert should overwrite the value for an existing key" $ do
      let t = Trie.insert @Word256 @_ 1 "newValue" $ Trie.insert 1 "oldValue" Trie.empty
      Trie.lookup 1 t `shouldBe` Just "newValue"

    it "singleton should create a trie with one key-value pair" $ do
      let t = Trie.singleton @Word256 @_ 1 "value"
      Trie.lookup 1 t `shouldBe` Just "value"

    it "empty should create an empty trie" $ do
      (Trie.lookup @Word256 @_ 1 Trie.empty :: Maybe Word256) `shouldBe` Nothing

    it "insert and lookup should work for multiple keys" $ do
      let t = Trie.insert @Word256 @_ 2 "value2" $ Trie.insert 1 "value1" Trie.empty
      Trie.lookup 1 t `shouldBe` Just "value1"
      Trie.lookup 2 t `shouldBe` Just "value2"

    it "insertWith should handle multiple keys correctly" $ do
      let t = Trie.insertWith @Word256 @_ (++) 2 "value2" $ Trie.insertWith (++) 1 "value1" Trie.empty
      Trie.lookup 1 t `shouldBe` Just "value1"
      Trie.lookup 2 t `shouldBe` Just "value2"

    it "insertWith should handle conflicts correctly" $ do
      let t = Trie.insertWith @Word256 @_ (++) 1 "value1" $ Trie.insertWith (++) 1 "value2" Trie.empty
      Trie.lookup 1 t `shouldBe` Just "value1value2"

    it "QuickCheck property: insert/lookup roundtrip for randomly generated tries" $ property $ \(keys :: [Word]) -> do
      let t = foldr (\k tr -> Trie.insert @Word256 @_ (fromIntegral k) (show k) tr) Trie.empty keys
      all (\k -> Trie.lookup (fromIntegral k) t == Just (show k)) keys `shouldBe` True
