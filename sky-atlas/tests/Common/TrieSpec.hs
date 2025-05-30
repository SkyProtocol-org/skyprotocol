{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
{-# OPTIONS_GHC -O0 #-} -- don't optimize errors away

module Common.TrieSpec (trieSpec) where

import Common.Types
import Common.Crypto
import Common.Trie
import qualified Common.Trie as TT
import Common.DA
import Common.TypesSpec

-- import Data.Bits (unsafeShiftL)
import Data.Functor.Identity (Identity (..))
-- import Data.String (String, IsString, fromString)
-- import Data.Text (pack, unpack)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
-- import Control.Exception
-- import Prelude (Int, putStrLn)

import PlutusTx.Prelude
-- import qualified PlutusTx.Prelude as P
-- import PlutusTx
-- import PlutusTx.Builtins
-- import PlutusTx.Builtins.Internal (BuiltinString (..))
-- import PlutusTx.List (zip)
import qualified PlutusTx.Show as PS
-- import PlutusTx.Utils

import qualified Debug.Trace as DT
import qualified GHC.Show as GS
import Data.Functor.Identity (Identity (..))
import qualified GHC.Base as GB
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

instance TrieHeight Integer
instance TrieHeightKey Integer Integer
instance TrieKey Integer

type S = Trie Identity Byte Bytes8 BuiltinString
type SR = TrieNodeRef Identity Byte Bytes8 BuiltinString

type T = Trie HashRef Integer Integer BuiltinString
type TR = TrieNodeRef HashRef Integer Integer BuiltinString

initialValues :: FromInt a => [(a,BuiltinString)]
initialValues = [(fromInt 13, "13"), (fromInt 34, "34"), (fromInt 1597, "1597")]

trieSpec :: Spec
trieSpec = describe "Spec.TrieSpec" $ do

  it "empty Trie" $ do
    let e0 = Empty :: TrieNodeF Byte Bytes8 (MessageEntry HashRef) (TrieNodeRef HashRef Byte Bytes8 (MessageEntry HashRef))
    let e1 = Fix (TrieNodeFL Empty) :: TrieNode Identity Byte Bytes8 Integer
    let e2 = LiftRef (Identity e1) :: TrieNodeRef Identity Byte Bytes8 Integer
    let e3 = TrieTop (-1) e2 :: Trie64 Identity Integer
    let trie0 = runIdentity (empty :: Identity (Trie64 Identity Integer))
    Byte 1 == Byte 2 `shouldBe` False
    e0 `shouldBeHex` "00"
    e1 `shouldBeHex` "00"
    e2 `shouldBeHex` "00"
    trie0 `shouldBeHex` "000000"
    e0 == e0 `shouldBe` True
    trie0 == e3 `shouldBe` True

  let rF :: TrieNodeF Byte Bytes8 BuiltinString SR -> SR = LiftRef . Identity . Fix . TrieNodeFL
  -- let fR :: SR -> TrieNodeF Byte Bytes8 BuiltinString SR = tfl . getFix . runIdentity . liftref
  let tp :: Integer -> Integer -> Integer -> [SR] -> TriePath Byte Bytes8 SR
      tp h k m d = TriePath h (fromInt k) (fromInt m) d
  let sd :: TrieStep Byte Bytes8 SR -> TriePath Byte Bytes8 SR -> Identity (TriePath Byte Bytes8 SR) = stepDown
  let su :: TrieStep Byte Bytes8 SR -> SR -> Identity SR = stepUp

  it "pathStep" $ do
    let
    runIdentity (pathStep $ tp 0 1597 1023 [rF $ Leaf "42"]) ==
      Just (SkipStep (Byte 9) (fromInt 573), tp 10 1 0 [rF $ Leaf "42"]) `shouldBe` True
    runIdentity (pathStep $ tp 5 0 30 [(rF $ Leaf "42"), (rF $ Leaf "69")]) ==
      Just (LeftStep (rF $ Leaf "42"), tp 6 0 15 [rF $ Leaf "69"]) `shouldBe` True

  it "stepDown" $ do
    runIdentity (sd (LeftStep (rF $ Leaf "42")) (tp 6 0 15 [rF $ Leaf "69"])) ==
      tp 5 0 30 [(rF $ Leaf "42"), (rF $ Leaf "69")] `shouldBe` True
    runIdentity (sd (SkipStep (Byte 9) (fromInt 573)) (tp 10 1 0 [rF $ Leaf "42"])) ==
      tp 0 1597 1023 [rF $ Leaf "42"] `shouldBe` True

  it "stepUp" $ do
    runIdentity (su (LeftStep (rF $ Leaf "1")) (rF $ Leaf "0")) ==
      rF (Branch (rF $ Leaf "0") (rF $ Leaf "1")) `shouldBe` True

  let rekey :: forall a . FromInt a => [(Integer, BuiltinString)] -> [(a, BuiltinString)]
      rekey = fmap (\ (x,y) -> (fromInt x, y))
  let ol l = (ofList . rekey $ l) :: Identity S
  let lo (t :: S) = TT.listOf t :: Identity [(Bytes8, BuiltinString)]
  let olt l = ofList l :: Identity T
  -- let lot (t :: T) = TT.listOf t :: Identity [(Integer, BuiltinString)]
  let roundtrip1 l1 = runIdentity (ol l1 >>= lo)
  let testListOfList l1 l2 = roundtrip1 l1 `shouldBe` rekey l2
  let testListOfList' l1 = testListOfList l1 l1
  it "list I/O 1" $ ol [] `shouldBeHex` "000000"
  it "list I/O 2" $ ol [(0,"a")] `shouldBeHex` "00010161"
  it "list I/O 3" $ ol [(0,"a"),(1,"b")] `shouldBeHex` "000202010001610162"
  it "list I/O 4" $ ol [(0,"a"),(1,"b"),(2,"c")] `shouldBeHex` "000302020100016101000162030000000000000000000163"
  it "list I/O 5" $ ol [(4611686018427387904,"")] `shouldBeHex` "0040033e400000000000000001"
  it "list I/O 2**62" $ PS.show (ol [(4611686018427387904,"")]) `shouldBe` "Identity (TrieTop 63 (LiftRef (Identity (Fix (TrieNodeFL Skip 62 4000000000000000 LiftRef (Identity (Fix (TrieNodeFL Leaf \"\"))))))))"
  it "list I/O 2**63" $ PS.show (ol [(9223372036854775808,"")]) `shouldBe` "Identity (TrieTop 64 (LiftRef (Identity (Fix (TrieNodeFL Skip 63 8000000000000000 LiftRef (Identity (Fix (TrieNodeFL Leaf \"\"))))))))"
  it "list to trie and back 1" $ testListOfList' []
  it "list to trie and back 2" $ testListOfList' [(13, "13"), (34, "34")]
  it "list to trie and back 3" $ testListOfList' initialValues
  it "list to trie and back 4" $ testListOfList [(34, "34"), (1597, "1597"), (13, "13")] initialValues
  it "list to trie and back 5" $ testListOfList [(1597, "1597"), (34, "34"), (13, "13")] initialValues
  it "list to trie and back 2**32" $ testListOfList' [(4294967296,"")]
  it "list to trie and back 2**33" $ testListOfList' [(8589934592,"")]
  it "list to trie and back 2**62" $ testListOfList' [(4611686018427387904,"")]
  it "list to trie and back 2**63-1" $ PS.show (roundtrip1 [(9223372036854775807,"")]) `shouldBe` "[(7fffffffffffffff,\"\")]"
  it "list to trie and back 2**63" $ PS.show (roundtrip1 [(9223372036854775808,"")]) `shouldBe` "[(8000000000000000,\"\")]"
  it "list to trie and back 2**63+1" $ PS.show (roundtrip1 [(9223372036854775809,"")]) `shouldBe` "[(8000000000000001,\"\")]"
  it "basic construction of the trie" $ do
    let testLookup l k v = runIdentity (ol l >>= lookup (fromInt k)) `shouldBe` v
    testLookup [] 3 Nothing
    testLookup [(0,"0")] 1 Nothing
    testLookup [(0,"0")] 0 $ Just "0"
    testLookup [(42,"a")] 34 Nothing
    testLookup [(42,"a")] 42 $ Just "a"
    testLookup initialValues 13 $ Just "13"
    testLookup initialValues 34 $ Just "34"
    testLookup initialValues 1597 $ Just "1597"
    testLookup initialValues 42 $ Nothing
    testLookup [(1,"1"),(0,"0")] 0 $ Just "0"
    testLookup [(9223372036854775808,"")] 9223372036854775808 $ Just ""

  it "testing creation of random tries" $ property $ \(listOfK :: [Bytes8]) -> do
    let someTrie :: S = runIdentity $ ofList $ zip listOfK $ fmap PS.show listOfK
    case listOfK of
      (k1 : k2 : ks) -> do
        runIdentity (lookup k1 someTrie) `shouldBe` Just (PS.show k1)
        runIdentity (lookup k2 someTrie) `shouldBe` Just (PS.show k2)
      (k1 : ks) -> do
        runIdentity (lookup k1 someTrie) `shouldBe` Just (PS.show k1)
      _ -> return ()

  it "testing inserting and removing" $ property $ \(listOfK :: [Bytes8]) -> do
    let someTrie :: S = runIdentity $ ofList $ zip listOfK $ fmap PS.show listOfK
    case listOfK of
      (k1 : k2 : ks) -> do
        let newTrie = runIdentity $ remove k1 someTrie >>= remove k2
        runIdentity (lookup k1 newTrie) `shouldBe` Nothing
        runIdentity (lookup k2 newTrie) `shouldBe` Nothing
        let newTrie' = runIdentity $ insert (PS.show k1) k1 newTrie >>= insert (PS.show k2) k2
        runIdentity (lookup k1 newTrie') `shouldBe` Just (PS.show k1)
        runIdentity (lookup k2 newTrie') `shouldBe` Just (PS.show k2)
      (k1 : ks) -> do
        let newTrie = runIdentity $ remove k1 someTrie
        runIdentity (lookup k1 newTrie) `shouldBe` Nothing
        let newTrie' = runIdentity $ insert (PS.show k1) k1 newTrie
        runIdentity (lookup k1 newTrie') `shouldBe` Just (PS.show k1)
      _ -> return ()

  it "should generate a proof, validate it, and compute the root hash correctly" $ do
    let t1 :: T
        t1 = runIdentity $ olt [(1,"value1"),(2,"value2")]
    let t1d :: DataHash = castDigest $ computeDigest t1
    let proof1 = runIdentity $ getMerkleProof 1 t1
    triePathHeight proof1 == 0 `shouldBe` True
    triePathKey proof1 == 1 `shouldBe` True
    let l1d :: DataHash = castDigest . getDigest . liftref . runIdentity $
                            ((rf $ Leaf "value1") :: Identity TR)
    let v1d = runIdentity $ applyMerkleProof l1d proof1
    v1d == t1d `shouldBe` True
