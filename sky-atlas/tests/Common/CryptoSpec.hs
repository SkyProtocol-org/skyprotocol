{-# LANGUAGE NoImplicitPrelude #-}

module Common.CryptoSpec where

import Common.Types
import Common.Crypto
import Common.Trie
import Common.TypesSpec
import PlutusTx.Prelude
import qualified PlutusTx.Show as PS
import Test.Hspec

cryptoSpec :: Spec
cryptoSpec = do
  describe "SkyCrypto" $ do
    it "simple tries" $ do
      let t1 = TrieTop 4 (2989 :: Integer)
      t1 `shouldBeHex2` "00050bad"
      PS.show t1 `shouldBe` "TrieTop 4 2989"
    it "simple hashes" $ do
      computeHash (hexB "") `shouldBeHex2` "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
      computeHash (hexB "00") `shouldBeHex2` "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"
      computeHash (ofHex "abcd" :: UInt16) `shouldBeHex2` "9606e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5"
