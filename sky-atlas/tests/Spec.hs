module Main (main) where

import Test.Hspec
import Common.TypesSpec (typesSpec)
import Common.CryptoSpec (cryptoSpec)
import Common.TrieSpec (trieSpec)
import Common.DaSpec (daSpec)
import Offchain.API (apiSpec)

main :: IO ()
main = hspec $ do
  typesSpec
  cryptoSpec
  trieSpec
  daSpec
  apiSpec
