{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.ContractSpec (contractSpec) where

import Contract.Bounty
import PlutusLedgerApi.V1.Interval (Interval (..), strictLowerBound, strictUpperBound)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx.Prelude
import Test.Tasty

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

contractSpec :: TestTree
contractSpec = testGroup "OnChain Contract Tests"
  [ testGroup "Bounty Contract"
    [ testCase "should validate timeout after deadline" $ do
        validateTimeout deadline txAfterDeadlineRange @?= True

    , testCase "should reject timeout before deadline" $ do
        validateTimeout deadline txBeforeDeadlineRange @?= False

    , testCase "should reject timeout in interval around deadline" $ do
        validateTimeout deadline txAroundDeadlineRange @?= False
    ]

  , testGroup "Bridge Contract"
    [ testCase "should validate timeout after deadline" $ do
        assertFailure "Not Yet Implemented"
    ]

  , testGroup "Sky Minting Policy"
    [ testCase "should validate minting with correct public key hash" $ do
        assertFailure "Not Yet Implemented"
    ]
  ]
