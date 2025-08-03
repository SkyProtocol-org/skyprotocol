{-# LANGUAGE NoImplicitPrelude #-}

module Common.CryptoSpec where

import Common.Crypto
import Common.Trie
import Common.Types
import Common.TypesSpec
import PlutusTx.Prelude
import PlutusTx.Show qualified as PS
import Test.Tasty
import Test.Tasty.HUnit

cryptoSpec :: TestTree
cryptoSpec =
  testGroup
    "SkyCrypto"
    [ testCase "to/from builtin data Bake2b_256" $ do
        let datum = ofHex @Hash "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
        let builtinDatum = toBuiltinData datum
        fromBuiltinData builtinDatum @?= Just datum,
      testCase "simple tries" $ do
        let t1 = TrieTop 4 (2989 :: Integer)
        t1 `shouldBeHex2` "00050bad"
        PS.show t1 @?= "TrieTop 4 2989",
      testCase "simple hashes" $ do
        computeDigest @Hash (hexB "") `shouldBeHex2` "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
        computeDigest @Hash (hexB "00") `shouldBeHex2` "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"
        computeDigest @Hash (ofHex "abcd" :: UInt16) `shouldBeHex2` "9606e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5"
    ]
