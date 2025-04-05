module Main (main) where

import Test.Hspec
--import Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec)
import Spec.SkyBaseSpec
import Spec.SkySpec

main :: IO ()
main = hspec $ do
  describe "SkyBase Tests" Spec.SkyBaseSpec.spec
  describe "Signature Tests" Spec.SkySpec.signatureSpec
  describe "Fingerprint Tests" Spec.SkySpec.fingerprintSpec
  describe "Simple SkyDA Tests" Spec.SkySpec.daSpec
{-  describe "Merkle Proof Tests" Spec.SkySpec.merkleSpec
  describe "Topic Tests" Spec.SkySpec.topicSpec
  describe "Bounty Tests" Spec.SkySpec.bountySpec
  describe "Bridge Tests" Spec.SkySpec.bridgeSpec
-}
