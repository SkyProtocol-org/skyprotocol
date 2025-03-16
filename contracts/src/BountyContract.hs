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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module BountyContract where

import SkyBridgeContract (BridgeNFTDatum (..), DataHash (..), getRefBridgeNFTDatumFromContext, pairHash)
import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash (..),
                           Credential(PubKeyCredential), addressCredential)
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
                          readBit, blake2b_256, consByteString, emptyByteString,
                          verifyEd25519Signature, appendByteString, sha2_256)
import Trie
import TrieUtils



data MerkleProof = MerkleProof
  { messageMetadata :: [ metadata
    
    keySize :: Integer,
    keyPath :: [Integer],
    siblingHashes :: [DataHash]
  }
  deriving (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''MerkleProof
PlutusTx.makeIsDataSchemaIndexed ''MerkleProof [('MerkleProof, 0)]

const2 :: BuiltinByteString
const2 = consByteString (2::Integer) emptyByteString

validate :: DataHash -> MerkleProof -> DataHash -> Bool
validate rootHash proof targetHash =
  rootHash == computeRootHash proof targetHash

computeRootHash :: MerkleProof -> DataHash -> DataHash
computeRootHash MerkleProof {..} targetHash =
  PlutusTx.foldr
    ( \(h, (DataHash hs)) (DataHash acc) ->
        if not (targetKey `readBit` h)
          then DataHash (blake2b_256 (appendByteString const2 (appendByteString acc hs)))
          else DataHash (blake2b_256 (appendByteString const2 (appendByteString hs acc)))
    )
    targetHash
    (PlutusTx.reverse $ PlutusTx.zip keyPath siblingHashes)

------------------------------------------------------------------------------
-- Initialization parameters for client contract
------------------------------------------------------------------------------

-- Use bytestring for now, might be integer later
data TopicID = TopicID BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''TopicID
PlutusTx.makeIsDataSchemaIndexed ''TopicID [('TopicID, 0)]

data ClientParams = ClientParams
  { bountyNFTCurrencySymbol :: CurrencySymbol
    -- ^ Unique currency symbol (hash of minting policy) of the bridge contract NFT
  , bountyClaimantPubKeyHash :: PubKeyHash
    -- ^ Credential of claimant (bounty prize can only be sent to this credential)
  , bountyTopicID :: TopicID
    -- ^ ID of topic in which data must be published
  , bountyMessageHash :: DataHash
    -- ^ Hash of data that must be proven to be present in DA
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientParams
PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

------------------------------------------------------------------------------
-- Redeemers for client contract
------------------------------------------------------------------------------

data ClientRedeemer
    = ClaimBounty
        { messageMetaData :: DataHash
          -- ^ metadata associated with the message
        , messageInDataProof :: MerkleProof
          -- ^ Proof that message is included in data directory
        , dataInTopicProof :: MerkleProof
          -- ^ Proof that data directory is included in topic
        , topicInDAProof :: MerkleProof
          -- ^ Proof that topic is included in DA root
        , topicCommitteeFingerprint :: DataHash
          -- ^ Fingerprint of topic committee multisig
        , mainCommitteeFingerprint :: DataHash
          -- ^ Fingerprint of main DA committee multisig
        }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientRedeemer
PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]

------------------------------------------------------------------------------
-- Client contract validator
------------------------------------------------------------------------------

metaDirKey :: BuiltinByteString
metaDirKey = consByteString 0 emptyByteString
dataDirKey :: BuiltinByteString
dataDirKey = consByteString 1 emptyByteString

-- Validator function without logic for fetching NFT from script context, for easy testing
clientTypedValidatorCore :: ClientRedeemer -> TopicID -> DataHash -> DataHash -> Bool
clientTypedValidatorCore claim@ClaimBounty{} bountyTopicID@(TopicID ti) bountyMessageHash nftTopHash =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions =
      [ -- The bounty's message hash is in the DA
        topHash PlutusTx.== nftTopHash
        -- topic ID matches
      , ti PlutusTx.== (targetKey (topicInDAProof claim))
        -- it's in the data directory
      , dataDirKey PlutusTx.== (targetKey (dataInTopicProof claim))
      ]
    -- Root hash for message in data directory
    dataRootHash = computeRootHash (messageInDataProof claim) bountyMessageHash
--    topicDataHash = pairHash (topicMetaDataHash claim) dataRootHash
    -- Root hash for data directory in topic
    topicRootHash = computeRootHash (dataInTopicProof claim) dataRootHash
    -- Root hash for topic in DA
    mainRootHash = computeRootHash (topicInDAProof claim) topicRootHash
    -- Top hash produced by claim
--    topicRootHash = computeRootHash (topicInDAProof claim) topicDataHash
    topHash = pairHash (mainCommitteeFingerprint claim) mainRootHash

-- Main validator function
clientTypedValidator ::
    ClientParams ->
    () ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator params () redeemer ctx =
    PlutusTx.and
      [ clientTypedValidatorCore redeemer (bountyTopicID params) (bountyMessageHash params) nftTopHash
      , allPaidToCredential
      ]
  where
    -- Top hash stored in NFT
    nftTopHash :: DataHash
    nftTopHash = case getRefBridgeNFTDatumFromContext (bountyNFTCurrencySymbol params) ctx of
                   Nothing -> PlutusTx.traceError "bridge NFT not found"
                   Just (BridgeNFTDatum topHash) -> topHash
    -- Bounty prize funds are sent to pre-configured bounty claimant
    outputs :: [TxOut]
    outputs = txInfoOutputs (scriptContextTxInfo ctx)
    allPaidToCredential :: Bool
    allPaidToCredential =
      PlutusTx.all (\o -> addressCredential (txOutAddress o)
                          PlutusTx.== PubKeyCredential (bountyClaimantPubKeyHash params))
                   outputs

------------------------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------------------------

{-# INLINEABLE clientUntypedValidator #-}
clientUntypedValidator :: ClientParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
clientUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( clientTypedValidator
            params
            () -- ignore the untyped datum, it's unused
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

clientValidatorScript ::
    ClientParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
clientValidatorScript params =
    $$(PlutusTx.compile [||clientUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
