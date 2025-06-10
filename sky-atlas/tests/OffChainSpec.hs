module Main (main) where

import OffChain.API (apiSpec)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "OffChain Tests"
  [ apiSpec
  ]
