module Main (main) where

import Test.Hspec
import Spec.SkySpec (signatureSpec, fingerprintSpec)

main :: IO ()
main = hspec $ do
  describe "Signature Tests" Spec.SkySpec.signatureSpec
  describe "Fingerprint Tests" Spec.SkySpec.fingerprintSpec
