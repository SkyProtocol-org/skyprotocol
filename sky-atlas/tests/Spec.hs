module Main (main) where

import Common.CryptoSpec (cryptoSpec)
import Common.DaSpec (daSpec)
import Common.TrieSpec (trieSpec)
import Common.TypesSpec (typesSpec)
import OffChain.API (apiSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  typesSpec
  cryptoSpec
  trieSpec
  daSpec
  apiSpec
