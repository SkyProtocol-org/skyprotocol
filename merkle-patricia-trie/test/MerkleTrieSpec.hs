{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MerkleTrieSpec (spec) where

import Data.Aeson (encodeFile)
import Data.Binary (decode, encode)
import Data.MerkleTrie
import Data.Trie qualified as Trie
import Data.WideWord (Word256)
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

instance Trie.TrieKey Word256 where
  type TrieHeight Word256 = Word8

dumpExample :: IO ()
dumpExample = do
  let t = Trie.insert @Word256 @String 1 "value1" $ Trie.insert 2 "value2" Trie.empty
      merkleTrie = merkelize t
      proof1 = proof 1 t
      proof2 = proof 2 t
  case (proof1, proof2) of
    (Just p1, Just p2) -> do
      let valid_p = mkProofWithHash (rootHash merkleTrie) p1
      let invalid_p = mkProofWithHash (rootHash merkleTrie) $ p2 {targetValue = "invalidValue"}
      encodeFile "valid_proof.json" valid_p
      encodeFile "invalid_proof.json" invalid_p
    _ -> pure ()

spec :: Spec
spec = beforeAll dumpExample $ do
  describe "MerkleTrie" $ do
    it "should generate a proof, validate it, and compute the root hash correctly" $ do
      let t = Trie.insert @Word256 @String 1 "value1" $ Trie.insert 2 "value2" Trie.empty
          merkleTrie = merkelize t
          proof1 = proof 1 t
          proof2 = proof 2 t
      case (proof1, proof2) of
        (Just p1, Just p2) -> do
          validate p1 (rootHash merkleTrie) `shouldBe` True
          validate p2 (rootHash merkleTrie) `shouldBe` True
        _ -> expectationFailure "Failed to generate proofs"

    it "should return Nothing for a proof of a key not in the trie" $ do
      let t = Trie.insert @Word256 @String 1 "value1" $ Trie.insert 2 "value2" Trie.empty
      proof 3 t `shouldBe` Nothing

    it "should fail to validate an incorrect proof" $ do
      let t = Trie.insert @Word256 @String 1 "value1" $ Trie.insert 2 "value2" Trie.empty
          merkleTrie = merkelize t
          proof1 = proof 1 t
          proof2 = proof 2 t
      case (proof1, proof2) of
        (Just p1, Just p2) -> do
          validate p1 (rootHash merkleTrie) `shouldBe` True
          validate p2 (rootHash merkleTrie) `shouldBe` True
          -- Modify proof1 to make it invalid
          let invalidProof = p1 {targetValue = "invalidValue"}
          validate invalidProof (rootHash merkleTrie) `shouldBe` False
        _ -> expectationFailure "Failed to generate proofs"
    it "decode . encode should equal id" $ do
      let t = Trie.insert @Word256 @String 1 "value1" Trie.empty
          proof1 = proof 1 t
      case proof1 of
        Just p -> do
          decode (encode p) `shouldBe` p
        _ -> expectationFailure "Failed to generate proofs"
