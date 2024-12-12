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

module BountyContract where

import SkyBridgeContract (BridgeNFTDatum (..), DataHash (..), getRefBridgeNFTDatumFromContext, pairHash)
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

------------------------------------------------------------------------------
-- Simplified Merkle Proof
------------------------------------------------------------------------------

-- Trivial form of Merkle proof for a left and a right data hash -
-- i.e. a one-level binary Merkle tree.
data SimplifiedMerkleProof =
  SimplifiedMerkleProof { left :: DataHash, right :: DataHash }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''SimplifiedMerkleProof
PlutusTx.makeIsDataSchemaIndexed ''SimplifiedMerkleProof [('SimplifiedMerkleProof, 0)]

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
        { messageInTopicProof :: SimplifiedMerkleProof
          -- ^ Proof that data is included in topic
        , topicInDAProof :: SimplifiedMerkleProof
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

-- top_hash = hash(main_committee_fingerprint ++ main_root_hash)
-- main_root_hash = hash(left_topic_top_hash ++ right_topic_top_hash)
-- left_topic_top_hash = hash(left_topic_id ++ left_committee_fingerprint ++ left_topic_root_hash)
-- (same for right topic committee)

clientTypedValidator ::
    ClientParams ->
    () ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator params () claim@ClaimBounty{} ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions =
      [ -- The bounty's message hash is in the topic
        hashInMerkleProof (messageInTopicProof claim) (bountyMessageHash params)
        -- The topic's top hash is in the DA
      , hashInMerkleProof (topicInDAProof claim) topicTopHash
        -- The claimed top hash matches the one stored in the NFT
      , topHash PlutusTx.== nftTopHash
      ]
    -- Root hash of topic trie produced by claim
    topicRootHash :: DataHash
    topicRootHash = merkleProofToDataHash (messageInTopicProof claim)
    -- Topic top hash produced by claim
    topicTopHash :: DataHash
    topicTopHash = makeTopicTopHash (bountyTopicID params) (topicCommitteeFingerprint claim) topicRootHash
    -- Main root hash produced by claim
    mainRootHash :: DataHash
    mainRootHash = merkleProofToDataHash (topicInDAProof claim)
    -- Top hash produced by claim
    topHash :: DataHash
    topHash = pairHash (mainCommitteeFingerprint claim) mainRootHash
    -- Top hash stored in NFT
    nftTopHash :: DataHash
    nftTopHash = case getRefBridgeNFTDatumFromContext (bountyNFTCurrencySymbol params) ctx of
                   Nothing -> PlutusTx.traceError "bridge NFT not found"
                   Just (BridgeNFTDatum th) -> th

-- Verify whether a (leaf) hash is included in a Merkle proof
hashInMerkleProof :: SimplifiedMerkleProof -> DataHash -> Bool
hashInMerkleProof (SimplifiedMerkleProof leftHash rightHash) hash =
  (hash PlutusTx.== leftHash PlutusTx.|| hash PlutusTx.== rightHash)

-- The topic top hash includes the topic ID, topic committee fingerprint, and topic root hash
makeTopicTopHash :: TopicID -> DataHash -> DataHash -> DataHash
makeTopicTopHash (TopicID topicID) (DataHash committeeFingerprint) (DataHash topicRootHash) =
  DataHash (sha2_256 (appendByteString topicID (appendByteString committeeFingerprint topicRootHash)))

-- Hashes a merkle proof to produce the root data hash
merkleProofToDataHash :: SimplifiedMerkleProof -> DataHash
merkleProofToDataHash (SimplifiedMerkleProof leftHash rightHash) =
  pairHash leftHash rightHash

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
