module Main (main) where

import OnChain.ContractSpec (contractSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  contractSpec
