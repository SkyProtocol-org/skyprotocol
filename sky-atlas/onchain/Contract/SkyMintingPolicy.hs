{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Contract.SkyMintingPolicy where

import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2 (PubKeyHash, ScriptContext (..), TxInfo (..))
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx
import PlutusTx.Prelude (Bool (..))
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
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef pkh
