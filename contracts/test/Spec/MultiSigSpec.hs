module Spec.MultiSigSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Arithmetic operations" $ do
    it "1 + 1 should be 2" $ do
      (1 + 1) `shouldBe` 2

    it "2 * 3 should be 6" $ do
      (2 * 3) `shouldBe` 6
