module Main (main) where

import OnChain.BountySpec (bountySpec)
import OnChain.ContractSpec (contractSpec)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "OnChain Tests"
  [ contractSpec
  , bountySpec
  ]
