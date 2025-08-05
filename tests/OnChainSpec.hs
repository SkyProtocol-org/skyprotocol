module Main (main) where

import OnChain.BountySpec (bountySpec)
import OnChain.BridgeSpec (bridgeSpec)
import OnChain.MintingPolicySpec (mintingPolicySpec)
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "OnChain Tests"
      [ bountySpec,
        mintingPolicySpec,
        bridgeSpec
      ]
