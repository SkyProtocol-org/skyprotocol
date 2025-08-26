{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Contract.Bridge where

import Common
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value
  ( AssetClass (..),
    assetClassValueOf,
  )
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (findDatum, getContinuingOutputs)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.List
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx
import Prelude qualified as HP

------------------------------------------------------------------------------
-- Datum Stored in Bridge NFT
------------------------------------------------------------------------------

newtype BridgeDatum = BridgeDatum
  { bridgeTopHash :: Blake2b_256
  }
  deriving (Eq, FromByteString, ToByteString) via BuiltinByteString
  deriving (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeDatum
PlutusTx.makeIsDataSchemaIndexed ''BridgeDatum [('BridgeDatum, 0)]

------------------------------------------------------------------------------
-- Initialization parameters for the bridge contract
------------------------------------------------------------------------------

-- The currency symbol is the unique identifier of the NFT currency (= hash of minting script)
newtype BridgeParams = BridgeParams
  { bridgeNFTCurrencySymbol :: CurrencySymbol
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeParams
PlutusTx.makeIsDataSchemaIndexed ''BridgeParams [('BridgeParams, 0)]

------------------------------------------------------------------------------
-- UpdateBridge Redeemer: Sent to contract to update bridge NFT
------------------------------------------------------------------------------

data BridgeRedeemer = UpdateBridge
  { bridgeSchema :: Blake2b_256,
    bridgeCommittee :: MultiSigPubKey,
    bridgeOldRootHash :: Blake2b_256,
    bridgeNewTopHash :: Blake2b_256,
    bridgeSig :: MultiSig -- signature over new top hash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeRedeemer
PlutusTx.makeIsDataSchemaIndexed ''BridgeRedeemer [('UpdateBridge, 0)]

------------------------------------------------------------------------------
-- NFT Utilities
------------------------------------------------------------------------------

-- Function to find an input UTXO with a specific CurrencySymbol
{-# INLINEABLE findInputByCurrencySymbol #-}
findInputByCurrencySymbol :: CurrencySymbol -> [TxInInfo] -> Maybe TxInInfo
findInputByCurrencySymbol targetSymbol inputs =
  let assetClass = AssetClass (targetSymbol, TokenName "SkyBridge")
      findSymbol :: TxInInfo -> Bool
      findSymbol txInInfo =
        assetClassValueOf (txOutValue (txInInfoResolved txInInfo)) assetClass == 1
   in find findSymbol inputs

-- Function to get a Datum from a TxOut, handling both inline data and hashed data
{-# INLINEABLE getDatumFromTxOut #-}
getDatumFromTxOut :: TxOut -> ScriptContext -> Maybe Datum
getDatumFromTxOut txOut ctx = case txOutDatum txOut of
  OutputDatumHash dh -> findDatum dh (scriptContextTxInfo ctx) -- Lookup the datum using the hash
  OutputDatum datum -> Just datum -- Inline datum is directly available
  NoOutputDatum -> Nothing -- No datum attached

-- Deserialize a serialized bridge NFT datum
getBridgeDatum :: Datum -> Maybe BridgeDatum
getBridgeDatum (Datum d) = PlutusTx.fromBuiltinData d

-- Given a script context, find the bridge NFT UTXO
getBridgeDatumFromContext :: CurrencySymbol -> ScriptContext -> Maybe BridgeDatum
getBridgeDatumFromContext currencySymbol scriptContext = do
  -- Find the input by currency symbol
  inputInfo <- findInputByCurrencySymbol currencySymbol (txInfoInputs (scriptContextTxInfo scriptContext))
  -- Get the transaction output from the input info
  let txOut = txInInfoResolved inputInfo -- This retrieves the TxOut from TxInInfo
  -- Get the datum from the transaction output
  datum <- getDatumFromTxOut txOut scriptContext
  -- Get the BridgeNFTDatum from the datum
  getBridgeDatum $ trace "I'm getting to getBridgeNFTDatum for old datum" datum

-- Given a transaction output extract its serialized bridge NFT datum
getBridgeDatumFromTxOut :: TxOut -> ScriptContext -> Maybe BridgeDatum
getBridgeDatumFromTxOut ownOutput ctx = do
  -- Get the Datum from the TxOut
  datum <- getDatumFromTxOut ownOutput ctx
  -- Extract the BridgeNFTDatum from the Datum
  getBridgeDatum $ trace "I'm getting to getBridgeNFTDatum for new datum" datum

-- Given a script context, find the bridge NFT UTXO
-- XXX copypasta for reference inputs, could probably be unified with getBridgeNFTDatumFromContext
getRefBridgeDatumFromContext :: CurrencySymbol -> ScriptContext -> Maybe BridgeDatum
getRefBridgeDatumFromContext currencySymbol scriptContext = do
  -- Find the input by currency symbol
  inputInfo <- findInputByCurrencySymbol currencySymbol (txInfoReferenceInputs (scriptContextTxInfo scriptContext))
  -- Get the transaction output from the input info
  let txOut = txInInfoResolved inputInfo -- This retrieves the TxOut from TxInInfo
  -- Get the datum from the transaction output
  datum <- getDatumFromTxOut txOut scriptContext
  -- Get the BridgeNFTDatum from the datum
  getBridgeDatum datum

------------------------------------------------------------------------------
-- Bridge Contract
------------------------------------------------------------------------------

-- Validates bridge transactions
bridgeTypedValidator ::
  BridgeParams ->
  () ->
  BridgeRedeemer ->
  ScriptContext ->
  Bool
bridgeTypedValidator params () redeemer ctx@(ScriptContext _txInfo _ _) =
  and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      -- Update the bridge state
      UpdateBridge daSchema daCommittee oldRootHash topHash sig ->
        [ -- Core validation, below
          traceBool
            "core validation passed"
            "core validation didn't pass"
            $ bridgeTypedValidatorCore
              daSchema
              daCommittee
              oldRootHash
              topHash
              sig
              oldTopHash,
          -- The NFT must be again included in the outputs
          traceBool "output has nft" "output doesn't have nft" outputHasNFT,
          -- The NFT's data must have been updated
          traceBool "nft updated" "nft isn't updated" $ nftUpdated newTopHash
        ]

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> PlutusTx.traceError "expected exactly one output"

    oldBridgeDatum :: BridgeDatum
    oldBridgeDatum = case getBridgeDatumFromContext (bridgeNFTCurrencySymbol params) ctx of
      Just datum -> datum
      Nothing -> PlutusTx.traceError "Can't find old bridge datum"

    newBridgeDatum :: BridgeDatum
    newBridgeDatum = case getBridgeDatumFromTxOut ownOutput ctx of
      Just datum -> datum
      Nothing -> PlutusTx.traceError "Can't find new bridge datum"

    oldTopHash :: Blake2b_256
    oldTopHash = bridgeTopHash oldBridgeDatum

    newTopHash :: Blake2b_256
    newTopHash = bridgeTopHash newBridgeDatum

    -- The output NFT UTXO's datum must match the new values for the root hashes
    nftUpdated :: Blake2b_256 -> Bool
    nftUpdated topHash =
      traceBool "new top hash = old top hash" "new top hash != old top hash" $ newTopHash == topHash

    -- There must be exactly one output UTXO with our NFT's unique currency symbol
    outputHasNFT :: Bool
    outputHasNFT =
      let assetClass = AssetClass (bridgeNFTCurrencySymbol params, TokenName "SkyBridge")
       in assetClassValueOf (txOutValue ownOutput) assetClass == 1

-- Core validation function, for easy testing
bridgeTypedValidatorCore :: Blake2b_256 -> MultiSigPubKey -> Blake2b_256 -> Blake2b_256 -> MultiSig -> Blake2b_256 -> Bool
bridgeTypedValidatorCore daSchema daCommittee daData newTopHash sig oldTopHash =
  traceBool
    "old top hash = computed top hash"
    "old top hash != computed top hash"
    ( oldTopHash
        == computedOldTopHash
    )
    && traceBool
      "multi sig is valid"
      "multi sig isn't valid"
      (multiSigValid daCommittee newTopHash sig) -- this doesn't overspend the cpu budget
  where
    -- \^ The new top hash must be signed by the committee
    daCommitteeFingerprint :: Blake2b_256
    daCommitteeFingerprint = computeDigest daCommittee -- this does overspend the cpu budget
    computedOldDaMetaData :: Blake2b_256
    computedOldDaMetaData = computeDigest (daSchema, daCommitteeFingerprint)
    -- \| The old top hash must be the hash of the concatenation of committee fingerprint
    -- and old root hash
    computedOldTopHash :: Blake2b_256
    computedOldTopHash = computeDigest (computedOldDaMetaData, daData)

------------------------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------------------------

{-# INLINEABLE bridgeUntypedValidator #-}
bridgeUntypedValidator :: BridgeParams -> BuiltinData -> BuiltinUnit
bridgeUntypedValidator params ctx =
  PlutusTx.check
    ( bridgeTypedValidator
        params
        () -- ignore the untyped datum, it's unused
        redeemer
        scriptContext
    )
  where
    scriptContext = PlutusTx.unsafeFromBuiltinData ctx
    redeemer = PlutusTx.unsafeFromBuiltinData $ getRedeemer $ scriptContextRedeemer scriptContext

bridgeValidatorScript ::
  BridgeParams ->
  CompiledCode (BuiltinData -> BuiltinUnit)
bridgeValidatorScript params =
  $$(PlutusTx.compile [||bridgeUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef params
