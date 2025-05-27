module Main (main) where

import Test.Hspec
import Offchain.API (apiSpec)

main :: IO ()
main = hspec apiSpec
