{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
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

module SkyContracts where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf, flattenValue,
                                 assetClassValueOf, AssetClass (..))
import PlutusLedgerApi.V2 (CurrencySymbol, Value (..), Datum (..),
                           OutputDatum (..), ScriptContext (..),
                           TokenName (..), TxInfo (..), TxOut (..),
                           txOutDatum, TxInInfo, TxInfo,
                           from, to, txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, findDatum)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.Builtins (BuiltinByteString, equalsByteString, lessThanInteger,
                          verifyEd25519Signature, appendByteString, sha2_256)

--- CORE DATA TYPES

-- A hash
data DataHash = DataHash PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

instance Eq DataHash where
    (DataHash dh1) == (DataHash dh2) = equalsByteString dh1 dh2
instance PlutusTx.Eq DataHash where
    (DataHash dh1) == (DataHash dh2) = equalsByteString dh1 dh2

-- Hashes the concatenation of a pair of hashes
pairHash :: DataHash -> DataHash -> DataHash
pairHash (DataHash a) (DataHash b) = DataHash (sha2_256 (a `appendByteString` b))

-- A public key
data PubKey = PubKey PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

instance Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2
instance PlutusTx.Eq PubKey where
    (PubKey pk1) == (PubKey pk2) = equalsByteString pk1 pk2

-- List of data operators that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey [PubKey] Integer
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- A single signature by a single data operator public key
data SingleSig = SingleSig PubKey PlutusTx.BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

instance Eq SingleSig where
  (SingleSig pubKey1 sig1) == (SingleSig pubKey2 sig2) = pubKey1 == pubKey2 && sig1 == sig2
instance PlutusTx.Eq SingleSig where
  (SingleSig pubKey1 sig1) == (SingleSig pubKey2 sig2) = pubKey1 == pubKey2 && sig1 == sig2

-- Signatures produced by data operators for top hash, must be in same order as multi-sig pubkeys
data MultiSig = MultiSig [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''DataHash [('DataHash, 0)]
PlutusTx.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''SingleSig [('SingleSig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSig [('MultiSig, 0)]
PlutusTx.makeLift ''DataHash
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''SingleSig
PlutusTx.makeLift ''MultiSig

-- Data stored in the bridge NFT UTXO's datum
data BridgeNFTDatum = BridgeNFTDatum
  { bridgeNFTTopHash :: DataHash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

instance Eq BridgeNFTDatum where
  (BridgeNFTDatum th1) == (BridgeNFTDatum th2) = th1 == th2
instance PlutusTx.Eq BridgeNFTDatum where
  (BridgeNFTDatum th1) == (BridgeNFTDatum th2) = th1 == th2

PlutusTx.makeLift ''BridgeNFTDatum
PlutusTx.makeIsDataSchemaIndexed ''BridgeNFTDatum [('BridgeNFTDatum, 0)]

-- Initialization parameters for the bridge contract:
-- The currency symbol is the unique identifier of the NFT currency (= hash of minting script)
data BridgeParams = BridgeParams
  { bridgeNFTCurrencySymbol :: CurrencySymbol
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeParams
PlutusTx.makeIsDataSchemaIndexed ''BridgeParams [('BridgeParams, 0)]

-- Updates the bridge NFT
data BridgeRedeemer = UpdateBridge
  { bridgeCommittee :: MultiSigPubKey
  , bridgeOldDataHash :: DataHash
  , bridgeNewTopHash :: DataHash
  , bridgeSig :: MultiSig -- signature over new top hash
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''BridgeRedeemer
PlutusTx.makeIsDataSchemaIndexed ''BridgeRedeemer [('UpdateBridge, 0)]

--- UTILITIES

-- Function to find an input UTXO with a specific CurrencySymbol
findInputByCurrencySymbol :: CurrencySymbol -> [TxInInfo] -> Maybe TxInInfo
findInputByCurrencySymbol targetSymbol inputs =
    let assetClass = AssetClass (targetSymbol, TokenName "SkyBridge")
        findSymbol :: TxInInfo -> Bool
        findSymbol txInInfo =
          assetClassValueOf (txOutValue (txInInfoResolved txInInfo)) assetClass PlutusTx.== 1
    in PlutusTx.find (findSymbol) inputs

-- Function to get a Datum from a TxOut, handling both inline data and hashed data
getDatumFromTxOut :: TxOut -> ScriptContext -> Maybe Datum
getDatumFromTxOut txOut ctx = case txOutDatum txOut of
    OutputDatumHash dh -> findDatum dh (scriptContextTxInfo ctx)  -- Lookup the datum using the hash
    OutputDatum datum -> Just datum  -- Inline datum is directly available
    NoOutputDatum -> Nothing  -- No datum attached

-- Deserialize a serialized bridge NFT datum
getBridgeNFTDatum :: Datum -> Maybe BridgeNFTDatum
getBridgeNFTDatum (Datum d) = PlutusTx.fromBuiltinData d

-- Given a script context, find the bridge NFT UTXO
getBridgeNFTDatumFromContext :: CurrencySymbol -> ScriptContext -> Maybe BridgeNFTDatum
getBridgeNFTDatumFromContext currencySymbol scriptContext = do
    -- Find the input by currency symbol
    inputInfo <- findInputByCurrencySymbol currencySymbol (txInfoInputs (scriptContextTxInfo scriptContext))
    -- Get the transaction output from the input info
    let txOut = txInInfoResolved inputInfo  -- This retrieves the TxOut from TxInInfo
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
    let txOut = txInInfoResolved inputInfo  -- This retrieves the TxOut from TxInInfo
    -- Get the datum from the transaction output
    datum <- getDatumFromTxOut txOut scriptContext
    -- Get the BridgeNFTDatum from the datum
    getBridgeNFTDatum datum

--- BRIDGE CONTRACT

-- Validates bridge transactions
bridgeTypedValidator ::
    BridgeParams ->
    () ->
    BridgeRedeemer ->
    ScriptContext ->
    Bool
bridgeTypedValidator params () redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        -- Update the bridge state
        UpdateBridge committee oldDataHash newTopHash sig ->
            [
              -- The top hash must be signed by the committee
              multiSigValid committee newTopHash sig,
              -- The NFT must be again included in the outputs
              outputHasNFT,
              -- The NFT's data must have been updated
              nftUpdated newTopHash,
              -- The hash of pair(multisig-hash, old-data-hash) must be = old-top-hash
              oldTopHashMatches committee oldDataHash 
            ]

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> PlutusTx.traceError "expected exactly one output"

    -- There must be exactly one output UTXO with our NFT's unique currency symbol
    outputHasNFT :: Bool
    outputHasNFT =
      let assetClass = (AssetClass ((bridgeNFTCurrencySymbol params), TokenName "SkyBridge")) in
      assetClassValueOf (txOutValue ownOutput) assetClass PlutusTx.== 1

    bridgeNFTDatum :: Maybe BridgeNFTDatum
    bridgeNFTDatum = getBridgeNFTDatumFromTxOut ownOutput ctx

    oldTopHashMatches :: MultiSigPubKey -> DataHash -> Bool
    oldTopHashMatches committee oldDataHash =
      let (Just (BridgeNFTDatum oldTopHash)) = bridgeNFTDatum in
        oldTopHash PlutusTx.== pairHash oldDataHash (multiSigToDataHash committee)

    -- The NFT UTXO's datum must match the new values for the root hashes
    nftUpdated :: DataHash -> Bool
    nftUpdated newTopHash =
      bridgeNFTDatum PlutusTx.== Just (BridgeNFTDatum newTopHash)

-- Function that checks if a SingleSig is valid
singleSigValid :: DataHash -> SingleSig -> Bool
singleSigValid (DataHash topHash) (SingleSig (PubKey pubKey) sig) =
  verifyEd25519Signature pubKey topHash sig

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: MultiSigPubKey -> DataHash -> MultiSig -> Bool
multiSigValid (MultiSigPubKey pubKeys minSigs) topHash (MultiSig singleSigs) =
  let -- Extract the public keys from the SingleSig values
      pubKeysInSignatures = PlutusTx.map (\(SingleSig pubKey _) -> pubKey) singleSigs
      -- Check for duplicates by comparing the list to its nub version
      noDuplicates = pubKeysInSignatures PlutusTx.== PlutusTx.nub pubKeysInSignatures
  in if not noDuplicates
     then False -- Duplicates found, return False
     else let -- Filter for valid signatures from required public keys
              validSignatures = PlutusTx.filter (\ss@(SingleSig pubKey sig) -> pubKey `PlutusTx.elem` pubKeys && singleSigValid topHash ss) singleSigs
          in PlutusTx.length validSignatures PlutusTx.>= minSigs

-- Create fingerprint of a multisig pubkey
multiSigToDataHash :: MultiSigPubKey -> DataHash
multiSigToDataHash (MultiSigPubKey pubKeys _) = 
    let -- Step 1: Concatenate the public keys manually
        concatenated = concatPubKeys pubKeys
        -- Step 2: Apply sha2_256 to the concatenated byte string
        hashed = sha2_256 concatenated
    in DataHash hashed

-- Helper function to concatenate a list of PubKey byte strings
concatPubKeys :: [PubKey] -> PlutusTx.BuiltinByteString
concatPubKeys (PubKey pk : rest) = -- assume at least one pubkey
    let restConcatenated = concatPubKeys rest
    in appendByteString pk restConcatenated

