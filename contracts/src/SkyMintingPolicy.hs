{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module SkyMintingPolicy where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2 (PubKeyHash, ScriptContext (..), TxInfo (..))
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

type SkyMintingParams = PubKeyHash
type SkyMintingRedeemer = ()

{-# INLINEABLE skyTypedMintingPolicy #-}
skyTypedMintingPolicy ::
  SkyMintingParams ->
  SkyMintingRedeemer ->
  ScriptContext ->
  Bool
skyTypedMintingPolicy pkh _redeemer ctx =
  txSignedBy txInfo pkh PlutusTx.&& mintedExactlyOneToken
  where
    txInfo = scriptContextTxInfo ctx
    mintedExactlyOneToken = case flattenValue (txInfoMint txInfo) of
      [(currencySymbol, _tokenName, quantity)] ->
        currencySymbol PlutusTx.== ownCurrencySymbol ctx PlutusTx.&& quantity PlutusTx.== 1
      _ -> False

skyUntypedMintingPolicy ::
  SkyMintingParams ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
skyUntypedMintingPolicy pkh redeemer ctx =
  PlutusTx.check
    ( skyTypedMintingPolicy
        pkh
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

skyMintingPolicyScript ::
  SkyMintingParams ->
  CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
skyMintingPolicyScript pkh =
  $$(PlutusTx.compile [||skyUntypedMintingPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pkh
