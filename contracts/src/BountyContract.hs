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

import SkyBridgeContract (BridgeNFTDatum (..), DataHash, getRefBridgeNFTDatumFromContext, pairHash)
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
  , bountyTargetHash :: DataHash
    -- ^ Hash of data that must be proven to be present in DA
  , bountyTopicID :: TopicID
    -- ^ ID of topic in which data must be published
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

clientTypedValidator ::
    ClientParams ->
    () ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator params () redeemer ctx@(ScriptContext txInfo _) =
    PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
        -- Claim the bounty
        ClaimBounty proof multiSigPubKeyHash ->
            [
              -- The Merkle proof must match the root hash stored in the NFT
              merkleProofValid ctx (bountyNFTCurrencySymbol params) (bountyTargetHash params) multiSigPubKeyHash proof
            ]

------------------------------------------------------------------------------
-- Merkle Proof Validation
------------------------------------------------------------------------------

-- Verify that merkle proof is valid by looking up NFT UTXO in the script context
merkleProofValid :: ScriptContext -> CurrencySymbol -> DataHash -> DataHash -> SimplifiedMerkleProof -> Bool
merkleProofValid ctx csym targetHash multiSigPubKeyHash proof =
  case getRefBridgeNFTDatumFromContext csym ctx of
    Nothing -> PlutusTx.traceError "bridge NFT not found"
    Just (BridgeNFTDatum topHash) -> merkleProofNFTHashValid topHash targetHash multiSigPubKeyHash proof

-- Hashes a merkle proof to produce the root data hash
merkleProofToDataHash :: SimplifiedMerkleProof -> DataHash
merkleProofToDataHash (SimplifiedMerkleProof leftHash rightHash) =
  pairHash leftHash rightHash

-- Computes the top hash from the data trie hash and the committe hash
topHash :: DataHash -> DataHash -> DataHash
topHash trieHash multiSigPubKeyHash =
  pairHash trieHash multiSigPubKeyHash

-- The main function to validate the Merkle proof against the root
-- data hash stored in the NFT: Check that the merkle proof and
-- multisig hash hashes to the top hash, and either the left or right
-- child of the merkle proof is the bounty's target data hash.
merkleProofNFTHashValid :: DataHash -> DataHash -> DataHash -> SimplifiedMerkleProof -> Bool
merkleProofNFTHashValid nftTopHash targetHash multiSigPubKeyHash proof@(SimplifiedMerkleProof leftHash rightHash) =
  let proofHash = merkleProofToDataHash proof in
    (topHash proofHash multiSigPubKeyHash) PlutusTx.== nftTopHash PlutusTx.&&
    (targetHash PlutusTx.== leftHash PlutusTx.|| targetHash PlutusTx.== rightHash)

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
