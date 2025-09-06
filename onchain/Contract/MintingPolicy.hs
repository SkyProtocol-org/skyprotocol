{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Contract.MintingPolicy where

import GHC.Generics (Generic)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx
import PlutusTx.AssocMap as AM
import PlutusTx.Blueprint
import PlutusTx.List qualified as List
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx
import Prelude qualified as HP

newtype SkyMintingParams = SkyMintingParams { smpPubKeyHash :: PubKeyHash }
  deriving newtype (Generic, HP.Show)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''SkyMintingParams
PlutusTx.makeIsDataSchemaIndexed ''SkyMintingParams [('SkyMintingParams, 0)]

{-# INLINEABLE skyTypedMintingPolicy #-}
skyTypedMintingPolicy ::
  SkyMintingParams ->
  () ->
  ScriptContext ->
  Bool
skyTypedMintingPolicy (SkyMintingParams pkh) _redeemer ctx =
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
skyUntypedMintingPolicy smp ctx =
  PlutusTx.check
    ( skyTypedMintingPolicy
        smp
        ()
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

skyMintingPolicyScript ::
  SkyMintingParams ->
  CompiledCode (BuiltinData -> BuiltinUnit)
skyMintingPolicyScript smp =
  $$(PlutusTx.compile [||skyUntypedMintingPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef smp
