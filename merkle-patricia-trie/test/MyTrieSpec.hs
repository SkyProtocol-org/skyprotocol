{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MyTrieSpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.MyTrie
import Data.Utils
import Data.Internal.RecursionSchemes
import Data.WideWord (Word256)
import Data.Word (Word64, Word8)
import Debug.Trace
import Test.Hspec
import Test.QuickCheck hiding (listOf)
import Prelude hiding (lookup)

type S = Trie Identity Word8 Word64 String
type SR = TrieNodeRef Identity Word8 Word64 String

type T = Trie Blake2b_256_Ref Word8 Word256 String
type TR = TrieNodeRef Blake2b_256_Ref Word8 Word256 String

initialValues = [(13, "13"), (34, "34"), (1597, "1597")]

spec :: Spec
spec = describe "MyTrie" $ do

  let rF :: TrieNodeF Word8 Word64 String SR -> SR = Lift . Identity . In . TrieNodeFL
  let fR :: SR -> TrieNodeF Word8 Word64 String SR = tfl . out . runIdentity . lifted
  let tp :: Int -> Word64 -> Word64 -> [SR] -> TriePath Word8 Word64 SR = TriePath
  let sd :: TrieStep Word8 Word64 SR -> TriePath Word8 Word64 SR -> Identity (TriePath Word8 Word64 SR) = stepDown
  let su :: TrieStep Word8 Word64 SR -> SR -> Identity SR = stepUp

  it "pathStep" $ do
    runIdentity (pathStep $ tp 0 1597 1023 [rF $ Leaf "42"]) `shouldBe`
      Just (SkipStep 9 573, tp 10 1 0 [rF $ Leaf "42"])
    runIdentity (pathStep $ tp 5 0 30 [(rF $ Leaf "42"), (rF $ Leaf "69")]) `shouldBe`
      Just (LeftStep (rF $ Leaf "42"), tp 6 0 15 [rF $ Leaf "69"])

  it "stepDown" $ do
    runIdentity (sd (LeftStep (rF $ Leaf "42")) (tp 6 0 15 [rF $ Leaf "69"])) `shouldBe`
      tp 5 0 30 [(rF $ Leaf "42"), (rF $ Leaf "69")]
    runIdentity (sd (SkipStep 9 573) (tp 10 1 0 [rF $ Leaf "42"])) `shouldBe`
      tp 0 1597 1023 [rF $ Leaf "42"]

  it "stepUp" $ do
    runIdentity (su (LeftStep (rF $ Leaf "1")) (rF $ Leaf "0")) `shouldBe`
      rF (Branch (rF $ Leaf "0") (rF $ Leaf "1"))

  let ol l = ofList l :: Identity S
  let lo (t :: S) = listOf t :: Identity [(Word64, String)]

  it "construction from trie should yield the same trie" $ do
    let testListOfList l1 l2 = runIdentity (ol l1 >>= lo) `shouldBe` l2
    let testListOfList' l1 = testListOfList l1 l1
    testListOfList' []
    testListOfList' [(13, "13"), (34, "34")]
    testListOfList' initialValues
    testListOfList [(34, "34"), (1597, "1597"), (13, "13")] initialValues
    testListOfList [(1597, "1597"), (34, "34"), (13, "13")] initialValues

  it "basic construction of the trie" $ do
    let testLookup l k v = runIdentity (ol l >>= lookup k) `shouldBe` v
    testLookup [] 3 Nothing
    testLookup [(0,"0")] 1 Nothing
    testLookup [(0,"0")] 0 $ Just "0"
    testLookup [(42,"a")] 34 Nothing
    testLookup [(42,"a")] 42 $ Just "a"
{-
    testLookup initialValues 13 $ Just "13"
    testLookup initialValues 34 $ Just "34"
    testLookup initialValues 1597 $ Just "1597"
    testLookup initialValues 42 $ Nothing

  it "testing creation of random tries" $ property $ \(listOfK :: [Word64]) -> do
    let someTrie :: S = runIdentity $ ofList $ zip (fmap fromIntegral listOfK) $ fmap show listOfK
    case listOfK of
      (k1 : k2 : ks) -> do
        runIdentity (lookup someTrie k1) `shouldBe` Just (show k1)
        runIdentity (lookup someTrie k2) `shouldBe` Just (show k2)
      (k1 : ks) -> do
        runIdentity (lookup someTrie k1) `shouldBe` Just (show k1)
      _ -> pure ()

  it "testing inserting and removing" $ property $ \(listOfK :: [Word64]) -> do
    let someTrie :: S = runIdentity $ ofList $ zip (fmap fromIntegral listOfK) $ fmap show listOfK
    case listOfK of
      (k1 : k2 : ks) -> do
        let newTrie = runIdentity $ remove k1 someTrie >>= remove k2
        runIdentity (lookup newTrie k1) `shouldBe` Nothing
        runIdentity (lookup newTrie k2) `shouldBe` Nothing
        let newTrie' = runIdentity $ insert (show k1) k1 newTrie >>= insert (show k2) k2
        runIdentity (lookup newTrie' k1) `shouldBe` Just (show k1)
        runIdentity (lookup newTrie' k2) `shouldBe` Just (show k2)
      (k1 : ks) -> do
        let newTrie = runIdentity $ remove k1 someTrie
        runIdentity (lookup newTrie k1) `shouldBe` Nothing
        let newTrie' = runIdentity $ insert (show k1) k1 newTrie
        runIdentity (lookup newTrie' k1) `shouldBe` Just (show k1)
      _ -> pure ()

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
