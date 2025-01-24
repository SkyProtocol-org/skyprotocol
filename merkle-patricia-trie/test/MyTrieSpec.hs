{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyTrieSpec (spec) where

import Debug.Trace

import Data.Utils
import Data.MyTrie as M

import Data.Functor.Identity (Identity (..))
import Data.WideWord (Word256)
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

type S = M.Trie Identity Word8 Word256 String
type T = M.Trie Blake2b_256_Ref Word8 Word256 String

spec :: Spec
spec = describe "MyTrie" $ do
  let l3 = [(13,"13"),(34,"34"),(1597,"1597")]
  it "should work" $ do
    let t0 :: S = runIdentity $ M.empty
        l0' = runIdentity $ M.listOf t0
    --    t3 :: S = runIdentity $ M.ofList l3
--        t3'' :: S = runIdentity $ M.insert "veni, vidi, vici" 1597 t3
--        l3' = runIdentity $ M.listOf t3
--        l3'' = runIdentity $ M.listOf t3''
    -- l3' `shouldBe` l3
    l0' `shouldBe` []
    -- l3'' `shouldBe` [(13,"13"),(34,"34"),(1597,"veni, vidi, vici")]

{-
    it "should generate a proof, validate it, and compute the root hash correctly" $ do
      let t = Trie.insert @Word256 @_ 1 "value1" $ Trie.insert 2 "value2" Trie.empty
          merkleTrie = merkelize t
          proof1 = proof 1 t
          proof2 = proof 2 t
      case (proof1, proof2) of
        (Just p1, Just p2) -> do
          validate p1 (rootHash merkleTrie) `shouldBe` True
          validate p2 (rootHash merkleTrie) `shouldBe` True
        _ -> expectationFailure "Failed to generate proofs"

    it "should return Nothing for a proof of a key not in the trie" $ do
      let t = Trie.insert @Word256 @_ 1 "value1" $ Trie.insert 2 "value2" Trie.empty
      proof 3 t `shouldBe` Nothing

    it "should fail to validate an incorrect proof" $ do
      let t = Trie.insert @Word256 @_ 1 "value1" $ Trie.insert 2 "value2" Trie.empty
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
-}

