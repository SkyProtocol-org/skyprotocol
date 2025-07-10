{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Contract.SkyBridge where

import Common
import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value
  ( AssetClass (..),
    assetClassValueOf,
  )
import PlutusLedgerApi.V2
  ( CurrencySymbol,
    Datum (..),
    OutputDatum (..),
    ScriptContext (..),
    TokenName (..),
    TxInInfo,
    TxInfo (..),
    TxOut (..),
    txInInfoResolved,
    txOutDatum,
  )
import PlutusLedgerApi.V2.Contexts (findDatum, getContinuingOutputs)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.List
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx

------------------------------------------------------------------------------
-- Datum Stored in Bridge NFT
------------------------------------------------------------------------------

newtype BridgeNFTDatum = BridgeNFTDatum
  { bridgeNFTTopHash :: Hash
  }
  deriving (Eq, FromByteString, ToByteString) via BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeNFTDatum
PlutusTx.makeIsDataSchemaIndexed ''BridgeNFTDatum [('BridgeNFTDatum, 0)]

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
  { bridgeSchema :: Hash,
    bridgeCommittee :: MultiSigPubKey,
    bridgeOldRootHash :: Hash,
    bridgeNewTopHash :: Hash,
    bridgeSig :: MultiSig -- signature over new top hash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- PlutusTx.makeLift ''BridgeRedeemer
PlutusTx.makeIsDataSchemaIndexed ''BridgeRedeemer [('UpdateBridge, 0)]

-- instance FromByteString BridgeRedeemer where
--   byteStringIn isTerminal = byteStringIn isTerminal <&> uncurry5 UpdateBridge

------------------------------------------------------------------------------
-- NFT Utilities
------------------------------------------------------------------------------

-- Function to find an input UTXO with a specific CurrencySymbol
findInputByCurrencySymbol :: CurrencySymbol -> [TxInInfo] -> Maybe TxInInfo
findInputByCurrencySymbol targetSymbol inputs =
  let assetClass = AssetClass (targetSymbol, TokenName "SkyBridge")
      findSymbol :: TxInInfo -> Bool
      findSymbol txInInfo =
        assetClassValueOf (txOutValue (txInInfoResolved txInInfo)) assetClass == 1
   in find findSymbol inputs

-- Function to get a Datum from a TxOut, handling both inline data and hashed data
getDatumFromTxOut :: TxOut -> ScriptContext -> Maybe Datum
getDatumFromTxOut txOut ctx = case txOutDatum txOut of
  OutputDatumHash dh -> findDatum dh (scriptContextTxInfo ctx) -- Lookup the datum using the hash
  OutputDatum datum -> Just datum -- Inline datum is directly available
  NoOutputDatum -> Nothing -- No datum attached

-- Deserialize a serialized bridge NFT datum
getBridgeNFTDatum :: Datum -> Maybe BridgeNFTDatum
-- getBridgeNFTDatum (Datum d) = PlutusTx.fromBuiltinData d
getBridgeNFTDatum (Datum d) = PlutusTx.fromBuiltinData d >>= maybeFromByteStringIn

-- Given a script context, find the bridge NFT UTXO
getBridgeNFTDatumFromContext :: CurrencySymbol -> ScriptContext -> Maybe BridgeNFTDatum
getBridgeNFTDatumFromContext currencySymbol scriptContext = do
  -- Find the input by currency symbol
  inputInfo <- findInputByCurrencySymbol currencySymbol (txInfoInputs (scriptContextTxInfo scriptContext))
  -- Get the transaction output from the input info
  let txOut = txInInfoResolved inputInfo -- This retrieves the TxOut from TxInInfo
  -- Get the datum from the transaction output
  datum <- getDatumFromTxOut txOut scriptContext
  -- Get the BridgeNFTDatum from the datum
  getBridgeNFTDatum datum

-- Given a transaction output extract its serialized bridge NFT datum
getBridgeNFTDatumFromTxOut :: TxOut -> ScriptContext -> Maybe BridgeNFTDatum
getBridgeNFTDatumFromTxOut ownOutput ctx = do
  -- Get the Datum from the TxOut
  datum <- getDatumFromTxOut ownOutput ctx
  -- Extract the BridgeNFTDatum from the Datum
  getBridgeNFTDatum datum

-- Given a script context, find the bridge NFT UTXO
-- XXX copypasta for reference inputs, could probably be unified with getBridgeNFTDatumFromContext
getRefBridgeNFTDatumFromContext :: CurrencySymbol -> ScriptContext -> Maybe BridgeNFTDatum
getRefBridgeNFTDatumFromContext currencySymbol scriptContext = do
  -- Find the input by currency symbol
  inputInfo <- findInputByCurrencySymbol currencySymbol (txInfoReferenceInputs (scriptContextTxInfo scriptContext))
  -- Get the transaction output from the input info
  let txOut = txInInfoResolved inputInfo -- This retrieves the TxOut from TxInInfo
  -- Get the datum from the transaction output
  datum <- getDatumFromTxOut txOut scriptContext
  -- Get the BridgeNFTDatum from the datum
  getBridgeNFTDatum datum

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
bridgeTypedValidator params () redeemer ctx@(ScriptContext _txInfo _) =
  and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      -- Update the bridge state
      UpdateBridge daSchema daCommittee oldRootHash newTopHash sig ->
        [ -- Core validation, below
          bridgeTypedValidatorCore
            daSchema
            daCommittee
            oldRootHash
            newTopHash
            sig
            oldNFTTopHash,
          -- The NFT must be again included in the outputs
          outputHasNFT,
          -- The NFT's data must have been updated
          nftUpdated newTopHash
        ]

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> PlutusTx.traceError "expected exactly one output"

    oldBridgeNFTDatum :: BridgeNFTDatum
    oldBridgeNFTDatum = fromJust $ getBridgeNFTDatumFromContext (bridgeNFTCurrencySymbol params) ctx

    newBridgeNFTDatum :: BridgeNFTDatum
    newBridgeNFTDatum = fromJust $ getBridgeNFTDatumFromTxOut ownOutput ctx

    oldNFTTopHash :: Hash
    oldNFTTopHash = bridgeNFTTopHash oldBridgeNFTDatum

    newNFTTopHash :: Hash
    newNFTTopHash = bridgeNFTTopHash newBridgeNFTDatum

    -- The output NFT UTXO's datum must match the new values for the root hashes
    nftUpdated :: Hash -> Bool
    nftUpdated newTopHash =
      newNFTTopHash == newTopHash

    -- There must be exactly one output UTXO with our NFT's unique currency symbol
    outputHasNFT :: Bool
    outputHasNFT =
      let assetClass = AssetClass (bridgeNFTCurrencySymbol params, TokenName "SkyBridge")
       in assetClassValueOf (txOutValue ownOutput) assetClass == 1

-- Core validation function, for easy testing
bridgeTypedValidatorCore :: Hash -> MultiSigPubKey -> Hash -> Hash -> MultiSig -> Hash -> Bool
bridgeTypedValidatorCore daSchema daCommittee daData newTopHash sig oldTopHash =
  multiSigValid daCommittee newTopHash sig
    &&
    -- \^ The new top hash must be signed by the committee
    oldTopHash
    == computedOldTopHash
  where
    -- \^ The old top hash must be the hash of the concatenation of committee fingerprint
    --   and old root hash
    daCommitteeFingerprint :: Hash
    daCommitteeFingerprint = computeDigest daCommittee
    computedOldDaMetaData :: Hash
    computedOldDaMetaData = computeDigest (daSchema, daCommitteeFingerprint)
    computedOldTopHash :: Hash
    computedOldTopHash = computeDigest (computedOldDaMetaData, daData)

------------------------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------------------------

{-# INLINEABLE bridgeUntypedValidator #-}
bridgeUntypedValidator :: BridgeParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
bridgeUntypedValidator params _datum redeemer ctx =
  PlutusTx.check
    ( bridgeTypedValidator
        params
        -- (BridgeParams "kek")
        () -- ignore the untyped datum, it's unused
        -- exampleRedeemer
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

bridgeValidatorScript ::
  BridgeParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
bridgeValidatorScript params =
  $$(PlutusTx.compile [||bridgeUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
