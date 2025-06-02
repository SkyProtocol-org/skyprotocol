module Main (main) where

import OffChain.API (apiSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  apiSpec