{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MyTrieSpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.MyTrie as M
import Data.Utils
import Data.WideWord (Word256)
import Data.Word (Word64, Word8)
import Debug.Trace
import Test.Hspec
import Test.QuickCheck

type S = M.Trie Identity Word8 Word64 String

type T = M.Trie Blake2b_256_Ref Word8 Word64 String

initialValues = [(13, "13"), (34, "34"), (1597, "1597")]

spec :: Spec
spec = describe "MyTrie" $ do
  it "basic construction of the trie" $ do
    let someTrie :: S = runIdentity $ M.ofList initialValues
        someValues = runIdentity $ M.listOf someTrie
    someValues `shouldBe` initialValues
    runIdentity (M.lookup someTrie 34) `shouldBe` Just "34"

  it "construction from trie should yield the same trie" $ do
    let emptyTrie :: S = runIdentity M.empty
        someTrie :: S = runIdentity $ M.ofList initialValues

    runIdentity (M.listOf emptyTrie) `shouldBe` []
    runIdentity (M.listOf someTrie) `shouldBe` initialValues

  it "testing creation of random tries" $ property $ \(listOfK :: [Word64]) -> do
    let someTrie :: S = runIdentity $ M.ofList $ zip (fmap fromIntegral listOfK) $ fmap show listOfK
    case listOfK of
      (k1 : k2 : ks) -> do
        runIdentity (M.lookup someTrie k1) `shouldBe` Just (show k1)
        runIdentity (M.lookup someTrie k2) `shouldBe` Just (show k2)
      (k1 : ks) -> do
        runIdentity (M.lookup someTrie k1) `shouldBe` Just (show k1)
      _ -> pure ()

  it "testing inserting and removing" $ property $ \(listOfK :: [Word64]) -> do
    let someTrie :: S = runIdentity $ M.ofList $ zip (fmap fromIntegral listOfK) $ fmap show listOfK
    case listOfK of
      (k1 : k2 : ks) -> do
        let newTrie = runIdentity $ M.remove k1 someTrie >>= M.remove k2
        runIdentity (M.lookup newTrie k1) `shouldBe` Nothing
        runIdentity (M.lookup newTrie k2) `shouldBe` Nothing
        let newTrie' = runIdentity $ M.insert (show k1) k1 newTrie >>= M.insert (show k2) k2
        runIdentity (M.lookup newTrie' k1) `shouldBe` Just (show k1)
        runIdentity (M.lookup newTrie' k2) `shouldBe` Just (show k2)
      (k1 : ks) -> do
        let newTrie = runIdentity $ M.remove k1 someTrie
        runIdentity (M.lookup newTrie k1) `shouldBe` Nothing
        let newTrie' = runIdentity $ M.insert (show k1) k1 newTrie
        runIdentity (M.lookup newTrie' k1) `shouldBe` Just (show k1)
      _ -> pure ()

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
