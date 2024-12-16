module Main (main) where

import Test.Hspec
import Spec.SkySpec (spec)

main :: IO ()
main = hspec $ do
  describe "Tests" Spec.SkySpec.spec
