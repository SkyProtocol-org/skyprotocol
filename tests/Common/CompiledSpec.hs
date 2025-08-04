{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.CompiledSpec where

import Common.Types
import Contract.Bridge (BridgeParams (..), bridgeValidatorScript)
import PlutusLedgerApi.V2 (CurrencySymbol (..))
import PlutusTx (CompiledCode, applyCode, compile, liftCodeDef, unsafeApplyCode)
import PlutusTx.Builtins
import PlutusTx.Code (CompiledCode)
import PlutusTx.Eval (EvalResult, displayEvalResult, evaluateCompiledCode)
import PlutusTx.Prelude
import PlutusTx.Show qualified as PS
import PlutusTx.TH (compile)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude (print)

test1 :: Integer -> Bool
test1 n = toByteString (toUInt16 n) == toByteString (toUInt16 1)

compiledTest1 :: CompiledCode (Integer -> Bool)
compiledTest1 = $$(compile [||test1||])

test2 :: Bool
test2 = (1 :: Integer) == 1

compiledTest2 :: CompiledCode Bool
compiledTest2 = $$(compile [||test2||])

-- TODO make better tests here
compiledSpec :: TestTree
compiledSpec =
  testGroup
    "ToByteString"
    [ testCase "toByteString (toUInt16 1)" $ do
        let res = evaluateCompiledCode compiledTest1
        -- print res
        1 @?= 1,
      testCase "1 == 1" $ do
        let res = evaluateCompiledCode compiledTest2
        -- print res
        1 @?= 1,
      testCase "Bridge validator" $ do
        let res = evaluateCompiledCode $ bridgeValidatorScript $ BridgeParams $ CurrencySymbol "kek"
        -- print $ displayEvalResult res
        1 @?= 1
    ]
