module Main (main) where

import Test.Hspec
--import Spec.SkySpec (signatureSpec, fingerprintSpec, merkleSpec, topicSpec, bountySpec, bridgeSpec)
import Spec.SkyBaseSpec
import Spec.TrieSpec
import Spec.SkySpec

main :: IO ()
main = hspec $ do
  describe "SkyBase Base Tests" Spec.SkyBaseSpec.baseSpec
  describe "SkyBase Crypto Tests" Spec.SkyBaseSpec.cryptoSpec
  describe "Signature Tests" Spec.SkySpec.signatureSpec
  describe "Fingerprint Tests" Spec.SkySpec.fingerprintSpec
  describe "Trie Tests" Spec.TrieSpec.spec
  describe "Simple SkyDA Tests" Spec.SkySpec.daSpec
{-
  describe "Topic Tests" Spec.SkySpec.topicSpec
  describe "Bounty Tests" Spec.SkySpec.bountySpec
  describe "Bridge Tests" Spec.SkySpec.bridgeSpec
-}
