{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.ContractSpec (contractSpec) where

import Contract.Bounty
import PlutusLedgerApi.V1.Interval (Interval (..), strictLowerBound, strictUpperBound)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx.Prelude
import Test.Hspec

deadline :: POSIXTime
deadline = 1000180800000 -- Sep 11th 2001

txBeforeDeadlineRange :: Interval POSIXTime
txBeforeDeadlineRange =
  Interval
    (strictLowerBound 999316800000) -- Sep 1st 2001
    (strictUpperBound 1000008000000) -- Sep 9th 2001

txAfterDeadlineRange :: Interval POSIXTime
txAfterDeadlineRange =
  Interval
    (strictLowerBound 1000267200000) -- Sep 12th 2001
    (strictUpperBound 1000872000000) -- Sep 19th 2001

txAroundDeadlineRange :: Interval POSIXTime
txAroundDeadlineRange =
  Interval
    (strictLowerBound 1000008000000) -- Sep 9th 2001
    (strictUpperBound 1000872000000) -- Sep 19th 2001

contractSpec :: Spec
contractSpec = do
  describe "OnChain Contract Tests" $ do
    describe "Bounty Contract" $ do
      it "should validate timeout after deadline" $ do
        validateTimeout deadline txAfterDeadlineRange `shouldBe` True

      it "should reject timeout before deadline" $ do
        validateTimeout deadline txBeforeDeadlineRange `shouldBe` False

      it "should reject timeout in interval around deadline" $ do
        validateTimeout deadline txAroundDeadlineRange `shouldBe` False

    describe "Bridge Contract" $ do
      it "should validate timeout after deadline" $ do
        pendingWith "Not Yet Implemented"

    describe "Sky Minting Policy" $ do
      it "should validate minting with correct public key hash" $ do
        pendingWith "Not Yet Implemented"
