{-# LANGUAGE TemplateHaskell #-}

module Contract.MintingPolicy where

import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx
import PlutusTx.AssocMap as AM
import PlutusTx.List qualified as List
import PlutusTx.Prelude
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
  List.and
    [ traceBool
        "Signed properly"
        "Signed improperly"
        (txSignedBy txInfo pkh),
      traceBool
        "Minted exactly one token"
        "Minted wrongly"
        mintedExactlyOneToken
    ]
  where
    txInfo = scriptContextTxInfo ctx
    mintedExactlyOneToken =
      case AM.toList . mintValueToMap $ txInfoMint txInfo of
        [(currencySymbol, tokenToAmount)] ->
          (currencySymbol == ownCurrencySymbol ctx)
            && ( case AM.toList tokenToAmount of
                   [(_token, amount)] -> amount == 1
                   _ -> False
               )
        _ -> False

skyUntypedMintingPolicy ::
  SkyMintingParams ->
  BuiltinData ->
  BuiltinUnit
skyUntypedMintingPolicy pkh ctx =
  PlutusTx.check
    ( skyTypedMintingPolicy
        pkh
        ()
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

skyMintingPolicyScript ::
  SkyMintingParams ->
  CompiledCode (BuiltinData -> BuiltinUnit)
skyMintingPolicyScript pkh =
  $$(PlutusTx.compile [||skyUntypedMintingPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef pkh
