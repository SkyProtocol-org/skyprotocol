{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

module Common.TrieSpec (trieSpec) where

import Common.Crypto
import Common.DA
import Common.Trie
import Common.Trie qualified as TT
import Common.Types
import Common.TypesSpec
import Data.Functor.Identity (Identity (..))
import PlutusTx.List
import PlutusTx.Prelude
import PlutusTx.Show qualified as PS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance TrieHeight Integer

instance TrieHeightKey Integer Integer

instance TrieKey Integer

type S = Trie Identity Byte Bytes8 BuiltinString

type SR = TrieNodeRef Identity Byte Bytes8 BuiltinString

type T = Trie (HashRef Hash) Integer Integer BuiltinString

type TR = TrieNodeRef (HashRef Hash) Integer Integer BuiltinString

initialValues :: (FromInt a) => [(a, BuiltinString)]
initialValues = [(fromInt 13, "13"), (fromInt 34, "34"), (fromInt 1597, "1597")]

trieSpec :: TestTree
trieSpec =
  testGroup "Spec.TrieSpec"
    $ [ testCase "empty Trie" $ do
          let e0 = Empty :: TrieNodeF Byte Bytes8 (MessageEntry (HashRef Hash)) (TrieNodeRef (HashRef Hash) Byte Bytes8 (MessageEntry (HashRef Hash)))
          let e1 = Fix (TrieNodeFL Empty) :: TrieNode Identity Byte Bytes8 Integer
          let e2 = LiftRef (Identity e1) :: TrieNodeRef Identity Byte Bytes8 Integer
          let e3 = TrieTop (-1) e2 :: Trie64 Identity Integer
          let trie0 = runIdentity (empty :: Identity (Trie64 Identity Integer))
          Byte 1 == Byte 2 @?= False
          e0 `shouldBeHex` "00"
          e1 `shouldBeHex` "00"
          e2 `shouldBeHex` "00"
          trie0 `shouldBeHex` "000000"
          e0 == e0 @?= True
          trie0 == e3 @?= True
      ]
    <> let rF :: TrieNodeF Byte Bytes8 BuiltinString SR -> SR = LiftRef . Identity . Fix . TrieNodeFL
           -- let fR :: SR -> TrieNodeF Byte Bytes8 BuiltinString SR = tfl . getFix . runIdentity . liftref
           tp :: Integer -> Integer -> Integer -> [SR] -> TriePath Byte Bytes8 SR
           tp h k m = TriePath h (fromInt k) (fromInt m)
           sd :: TrieStep Byte Bytes8 SR -> TriePath Byte Bytes8 SR -> Identity (TriePath Byte Bytes8 SR) = stepDown
           su :: TrieStep Byte Bytes8 SR -> SR -> Identity SR = stepUp
        in [ testCase "pathStep" $ do
               runIdentity (pathStep $ tp 0 1597 1023 [rF $ Leaf "42"])
                 == Just (SkipStep (Byte 9) (fromInt 573), tp 10 1 0 [rF $ Leaf "42"])
                 @?= True
               runIdentity (pathStep $ tp 5 0 30 [rF $ Leaf "42", rF $ Leaf "69"])
                 == Just (LeftStep (rF $ Leaf "42"), tp 6 0 15 [rF $ Leaf "69"])
                 @?= True,
             testCase "stepDown" $ do
               runIdentity (sd (LeftStep (rF $ Leaf "42")) (tp 6 0 15 [rF $ Leaf "69"]))
                 == tp 5 0 30 [rF $ Leaf "42", rF $ Leaf "69"]
                 @?= True
               runIdentity (sd (SkipStep (Byte 9) (fromInt 573)) (tp 10 1 0 [rF $ Leaf "42"]))
                 == tp 0 1597 1023 [rF $ Leaf "42"]
                 @?= True,
             testCase "stepUp" $ do
               runIdentity (su (LeftStep (rF $ Leaf "1")) (rF $ Leaf "0"))
                 == rF (Branch (rF $ Leaf "0") (rF $ Leaf "1"))
                 @?= True
           ]
             <> let rekey :: forall a. (FromInt a) => [(Integer, BuiltinString)] -> [(a, BuiltinString)]
                    rekey = fmap (\(x, y) -> (fromInt x, y))
                    ol l = (ofList . rekey $ l) :: Identity S
                    lo (t :: S) = TT.listOf t :: Identity [(Bytes8, BuiltinString)]
                    olt l = ofList l :: Identity T
                    -- lot (t :: T) = TT.listOf t :: Identity [(Integer, BuiltinString)]
                    roundtrip1 l1 = runIdentity (ol l1 >>= lo)
                    testListOfList l1 l2 = roundtrip1 l1 @?= rekey l2
                    testListOfList' l1 = testListOfList l1 l1
                 in [ testCase "list I/O 1" $ ol [] `shouldBeHex` "000000",
                      testCase "list I/O 2" $ ol [(0, "a")] `shouldBeHex` "00010161",
                      testCase "list I/O 3" $ ol [(0, "a"), (1, "b")] `shouldBeHex` "000202010001610162",
                      testCase "list I/O 4" $ ol [(0, "a"), (1, "b"), (2, "c")] `shouldBeHex` "000302020100016101000162030000000000000000000163",
                      testCase "list I/O 5" $ ol [(4611686018427387904, "")] `shouldBeHex` "0040033e400000000000000001",
                      testCase "list I/O 2**62" $ PS.show (ol [(4611686018427387904, "")]) @?= "Identity (TrieTop 63 (LiftRef (Identity (Fix (TrieNodeFL Skip 62 4000000000000000 LiftRef (Identity (Fix (TrieNodeFL Leaf \"\"))))))))",
                      testCase "list I/O 2**63" $ PS.show (ol [(9223372036854775808, "")]) @?= "Identity (TrieTop 64 (LiftRef (Identity (Fix (TrieNodeFL Skip 63 8000000000000000 LiftRef (Identity (Fix (TrieNodeFL Leaf \"\"))))))))",
                      testCase "list to trie and back 1" $ testListOfList' [],
                      testCase "list to trie and back 2" $ testListOfList' [(13, "13"), (34, "34")],
                      testCase "list to trie and back 3" $ testListOfList' initialValues,
                      testCase "list to trie and back 4" $ testListOfList [(34, "34"), (1597, "1597"), (13, "13")] initialValues,
                      testCase "list to trie and back 5" $ testListOfList [(1597, "1597"), (34, "34"), (13, "13")] initialValues,
                      testCase "list to trie and back 2**32" $ testListOfList' [(4294967296, "")],
                      testCase "list to trie and back 2**33" $ testListOfList' [(8589934592, "")],
                      testCase "list to trie and back 2**62" $ testListOfList' [(4611686018427387904, "")],
                      testCase "list to trie and back 2**63-1" $ PS.show (roundtrip1 [(9223372036854775807, "")]) @?= "[(7fffffffffffffff,\"\")]",
                      testCase "list to trie and back 2**63" $ PS.show (roundtrip1 [(9223372036854775808, "")]) @?= "[(8000000000000000,\"\")]",
                      testCase "list to trie and back 2**63+1" $ PS.show (roundtrip1 [(9223372036854775809, "")]) @?= "[(8000000000000001,\"\")]",
                      testCase "basic construction of the trie" $ do
                        let testLookup l k v = runIdentity (ol l >>= lookup (fromInt k)) @?= v
                        testLookup [] 3 Nothing
                        testLookup [(0, "0")] 1 Nothing
                        testLookup [(0, "0")] 0 $ Just "0"
                        testLookup [(42, "a")] 34 Nothing
                        testLookup [(42, "a")] 42 $ Just "a"
                        testLookup initialValues 13 $ Just "13"
                        testLookup initialValues 34 $ Just "34"
                        testLookup initialValues 1597 $ Just "1597"
                        testLookup initialValues 42 Nothing
                        testLookup [(1, "1"), (0, "0")] 0 $ Just "0"
                        testLookup [(9223372036854775808, "")] 9223372036854775808 $ Just "",
                      testProperty "testing creation of random tries" $ \(listOfK :: [Bytes8]) ->
                        let someTrie :: S = runIdentity $ ofList $ zip listOfK $ fmap PS.show listOfK
                         in case listOfK of
                              (k1 : k2 : _) ->
                                runIdentity (lookup k1 someTrie)
                                  == Just (PS.show k1)
                                  && runIdentity (lookup k2 someTrie)
                                  == Just (PS.show k2)
                              (k1 : _) ->
                                runIdentity (lookup k1 someTrie) == Just (PS.show k1)
                              _ -> True,
                      testProperty "testing inserting and removing" $ \(listOfK :: [Bytes8]) ->
                        let someTrie :: S = runIdentity $ ofList $ zip listOfK $ fmap PS.show listOfK
                         in case listOfK of
                              (k1 : k2 : _) ->
                                let newTrie = runIdentity $ remove k1 someTrie >>= remove k2
                                 in isNothing (runIdentity (lookup k1 newTrie))
                                      && isNothing (runIdentity (lookup k2 newTrie))
                                      && let newTrie' = runIdentity $ insert (PS.show k1) k1 newTrie >>= insert (PS.show k2) k2
                                          in runIdentity (lookup k1 newTrie')
                                               == Just (PS.show k1)
                                               && runIdentity (lookup k2 newTrie')
                                               == Just (PS.show k2)
                              (k1 : _) ->
                                let newTrie = runIdentity $ remove k1 someTrie
                                 in isNothing (runIdentity (lookup k1 newTrie))
                                      && let newTrie' = runIdentity $ insert (PS.show k1) k1 newTrie
                                          in runIdentity (lookup k1 newTrie') == Just (PS.show k1)
                              _ -> True,
                      testCase "should generate a proof, validate it, and compute the root hash correctly" $ do
                        let t1 :: T
                            t1 = runIdentity $ olt [(1, "value1"), (2, "value2")]
                            t1d :: Hash = computeDigest @Hash t1
                            proof1 = runIdentity $ getMerkleProof 1 t1
                            l1d :: Hash =
                              refDigest
                                . liftref
                                . runIdentity
                                $ ((rf $ Leaf "value1") :: Identity TR)
                            v1d = runIdentity $ applyMerkleProof l1d proof1
                        triePathHeight proof1 == 0 @?= True
                        triePathKey proof1 == 1 @?= True
                        v1d == t1d @?= True
                    ]
