module Main (main) where

import Test.Hspec
import Spec.MultiSigSpec (spec)

main :: IO ()
main = hspec $ do
  describe "Multisig Tests" Spec.MultiSigSpec.spec
