{-# LANGUAGE OverloadedStrings #-}

module Main where

import Contract.Bridge (BridgeParams (..), bridgeValidatorScript)
import PlutusCore.Pretty (prettyReadableSimple)
import PlutusLedgerApi.V2 (CurrencySymbol (..))
import PlutusTx.Code (getPir)
import PlutusTx.Prelude ()

main :: IO ()
main = do
  let code = prettyReadableSimple $ getPir $ bridgeValidatorScript $ BridgeParams $ CurrencySymbol "kek"
  print code
