module Main (main) where

import OnChain.BountySpec (bountySpec)
import OnChain.BridgeSpec (bridgeSpec)
import OnChain.MintingPolicySpec (mintingPolicySpec)
import Test.Tasty

main :: IO ()
main =
  -- since bountySpec is, essentially, the whole contract flow test,
  -- we're running tests sequentially, waiting for each to succeed
  -- if one of them fails, everything else will fail anyway
  defaultMain $
    sequentialTestGroup
      "OnChain Tests"
      AllSucceed
      [ mintingPolicySpec,
        bridgeSpec,
        bountySpec
      ]
