module Main (main) where

import Common.CryptoSpec (cryptoSpec)
import Common.DaSpec (daSpec, signatureSpec, fingerprintSpec)
import Common.TrieSpec (trieSpec)
import Common.TypesSpec (typesSpec)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Common Tests"
  [ typesSpec
  , cryptoSpec
  , trieSpec
  , signatureSpec
  , fingerprintSpec
  , daSpec
  ]
