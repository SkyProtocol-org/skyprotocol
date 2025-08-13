{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Contract.Bridge (BridgeParams (..), bridgeValidatorScript)
import Contract.MintingPolicy (skyMintingPolicyScript)
-- import qualified Data.Text.IO as TIO
import PlutusCore.Pretty (prettyReadableSimple)
import PlutusLedgerApi.V2 (CurrencySymbol (..))
import PlutusTx.Code (CompiledCode, getPir)

-- import PlutusTx.Eval (EvalResult, displayEvalResult, evaluateCompiledCode)

main :: IO ()
main = do
  let script = skyMintingPolicyScript ""
  let code = prettyReadableSimple $ getPir script
  print code

-- let res = evaluateCompiledCode script
-- TIO.putStrLn $ displayEvalResult res
