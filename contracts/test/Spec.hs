module Main (main) where

import Test.Hspec
--import Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec)
import Spec.SkySpec

main :: IO ()
main = hspec $ do
  describe "Signature Tests" Spec.SkySpec.signatureSpec
  describe "Fingerprint Tests" Spec.SkySpec.fingerprintSpec
{-  describe "Merkle Proof Tests" Spec.SkySpec.merkleSpec
  describe "Topic Tests" Spec.SkySpec.topicSpec
  describe "Bounty Tests" Spec.SkySpec.bountySpec
  describe "Bridge Tests" Spec.SkySpec.bridgeSpec
-}
