module Main (main) where

import OnChain.ContractSpec (contractSpec)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "OnChain Tests"
  [ contractSpec
  ]
