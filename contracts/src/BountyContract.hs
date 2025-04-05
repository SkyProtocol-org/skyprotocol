{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
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

import SkyBase
import Trie
import SkyCrypto
import SkyDA
import SkyBridgeContract (BridgeNFTDatum (..), getRefBridgeNFTDatumFromContext)

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash (..),
                           Credential(PubKeyCredential), addressCredential)
import PlutusLedgerApi.V1.Interval (contains, Interval, before, after)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf, flattenValue,
                                 assetClassValueOf, AssetClass (..))
import PlutusLedgerApi.V2 (CurrencySymbol, Value (..), Datum (..),
                           OutputDatum (..), ScriptContext (..),
                           TokenName (..), TxInfo (..), TxOut (..),
                           txOutDatum, TxInInfo, TxInfo,
                           from, to, txInInfoResolved)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, findDatum, scriptContextTxInfo, txInfoValidRange)
import PlutusTx.Prelude
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.Builtins (BuiltinByteString, equalsByteString, lessThanInteger,
                          readBit, blake2b_256, consByteString, emptyByteString,
                          verifyEd25519Signature, appendByteString, sha2_256)
import Data.Functor.Identity (Identity (..))

--PlutusTx.makeLift ''SkyDataProof
--PlutusTx.makeIsDataSchemaIndexed ''SkyDataProof [('SkyDataProof, 0)]


------------------------------------------------------------------------------
-- Initialization parameters for client contract
------------------------------------------------------------------------------

-- 64-bit ByteString
data TopicID = TopicID { getTopicID :: Bytes8 }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance ByteStringIn TopicID where
  byteStringIn = byteStringIn <&> TopicID
instance ByteStringOut TopicID where
  byteStringOut = byteStringOut . getTopicID

{-PlutusTx.makeLift ''TopicID
PlutusTx.makeIsDataSchemaIndexed ''TopicID [('TopicID, 0)]-}

type ClientParams = BuiltinByteString

data DecodedClientParams = DecodedClientParams
  { bountyNFTCurrencySymbol :: CurrencySymbol
    -- ^ Unique currency symbol (hash of minting policy) of the bridge contract NFT
  , bountyClaimantPubKeyHash :: PubKeyHash
    -- ^ Credential of claimant (bounty prize can only be sent to this credential)
  , bountyOffererPubKeyHash :: PubKeyHash
    -- ^ Credential of offerer (to whom to send back the bounty if not claimed before timeout)
  , bountyTopicID :: TopicID
    -- ^ ID of topic in which data must be published
  , bountyMessageHash :: DataHash
    -- ^ Hash of data that must be proven to be present in DA
  , bountyDeadline :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
getDecodedClientParams :: DecodedClientParams -> (CurrencySymbol, PubKeyHash, PubKeyHash, TopicID, DataHash, POSIXTime)
getDecodedClientParams (DecodedClientParams a b c d e f) = (a, b, c, d, e, f)
instance ByteStringIn DecodedClientParams where
  byteStringIn = byteStringIn <&> uncurry6 DecodedClientParams
instance ByteStringOut DecodedClientParams where
  byteStringOut = byteStringOut . getDecodedClientParams

--PlutusTx.makeLift ''ClientParams
--PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

------------------------------------------------------------------------------
-- Redeemers for client contract
------------------------------------------------------------------------------

data ClientRedeemer = ClaimBounty SkyDataProof | Timeout
instance ByteStringIn ClientRedeemer where
  byteStringIn = byteStringIn <&> \case
    Left proof -> ClaimBounty proof
    Right () -> Timeout

--PlutusTx.makeLift ''ClientRedeemer
--PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]
--PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('Timeout, 1)]

------------------------------------------------------------------------------
-- Client contract validator
------------------------------------------------------------------------------

-- Separating validation logic to make for easy testing
validateClaimBounty :: POSIXTime -> Interval POSIXTime -> DataHash -> TopicID -> SkyDataProof -> DataHash -> Bool
validateClaimBounty bountyDeadline txValidRange
                    messageHash (TopicID topicId) proof@SkyDataPath {..} daTopHash =
  -- Check if the current slot is within the deadline
  bountyDeadline `after` txValidRange &&
  -- The bounty's message hash is in the DA
  daTopHash == applySkyDataProof proof messageHash &&
  -- topic ID matches
  topicId == triePathKey pathTopicTriePath

validateTimeout :: POSIXTime -> Interval POSIXTime -> Bool
validateTimeout bountyDeadline txValidRange =
  bountyDeadline `before` txValidRange

-- Main validator function
clientTypedValidator ::
    DecodedClientParams ->
    () ->
    ClientRedeemer ->
    ScriptContext ->
    Bool
clientTypedValidator DecodedClientParams {..} () redeemer ctx =
  case redeemer of
    ClaimBounty proof ->
      validateClaimBounty bountyDeadline txValidRange
                          bountyMessageHash bountyTopicID proof daTopHash &&
      allPaidToCredential bountyClaimantPubKeyHash
    Timeout ->
      validateTimeout bountyDeadline txValidRange &&
      allPaidToCredential bountyOffererPubKeyHash
  where
    -- DA Top hash stored in NFT
    daTopHash :: DataHash
    daTopHash = case getRefBridgeNFTDatumFromContext bountyNFTCurrencySymbol ctx of
                  Nothing -> PlutusTx.traceError "bridge NFT not found"
                  Just (BridgeNFTDatum topHash) -> topHash
    -- Tx validity interval
    txValidRange = txInfoValidRange . scriptContextTxInfo $ ctx
    -- Bounty prize funds are sent to pre-configured address
    allPaidToCredential :: PubKeyHash -> Bool
    allPaidToCredential recipient =
      PlutusTx.all (\o -> addressCredential (txOutAddress o) == PubKeyCredential recipient) $
                   txInfoOutputs (scriptContextTxInfo ctx)

------------------------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------------------------

{-# INLINEABLE clientUntypedValidator #-}
clientUntypedValidator :: ClientParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
clientUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( clientTypedValidator
            (fromByteStringIn $ params)
            () -- ignore the untyped datum, it's unused
            (fromByteStringIn $ PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

clientValidatorScript ::
    ClientParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
clientValidatorScript params =
    $$(PlutusTx.compile [||clientUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
