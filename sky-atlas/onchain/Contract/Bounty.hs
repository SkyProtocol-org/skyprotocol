{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Contract.Bounty where

import Common
import Contract.SkyBridge (BridgeNFTDatum (..), getRefBridgeNFTDatumFromContext)
import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1
  ( Credential (PubKeyCredential),
    POSIXTime,
    PubKeyHash (..),
    addressCredential,
  )
import PlutusLedgerApi.V1.Interval (Interval, before, contains, to)
import PlutusLedgerApi.V2
  ( CurrencySymbol,
    ScriptContext (..),
    TxInfo (..),
    TxOut (..),
  )
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.List
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx

------------------------------------------------------------------------------
-- Initialization parameters for client contract
------------------------------------------------------------------------------

data ClientParams = ClientParams
  { -- | Unique currency symbol (hash of minting policy) of the bridge contract NFT
    bountyNFTCurrencySymbol :: CurrencySymbol,
    -- | Credential of claimant (bounty prize can only be sent to this credential)
    bountyClaimantPubKeyHash :: PubKeyHash,
    -- | Credential of offerer (to whom to send back the bounty if not claimed before timeout)
    bountyOffererPubKeyHash :: PubKeyHash,
    -- | ID of topic in which data must be published
    bountyTopicId :: TopicId,
    -- | Hash of data that must be proven to be present in DA
    bountyMessageHash :: Hash,
    bountyDeadline :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''ClientParams
PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

------------------------------------------------------------------------------
-- Redeemers for client contract
------------------------------------------------------------------------------

data ClientRedeemer = ClaimBounty (SkyDataProof Blake2b_256) | Timeout

-- PlutusTx.unstableMakeIsData ''ClientRedeemer
-- PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0), ('Timeout, 1)]

------------------------------------------------------------------------------
-- Client contract validator
------------------------------------------------------------------------------

-- Separating validation logic to make for easy testing
validateClaimBounty :: POSIXTime -> Interval POSIXTime -> Hash -> TopicId -> SkyDataProof Blake2b_256 -> Hash -> Bool
validateClaimBounty
  bountyDeadline
  txValidRange
  messageHash
  topicId
  proof@SkyDataPath {..}
  daTopHash =
    -- Check if the current slot is within the deadline
    to bountyDeadline
      `contains` txValidRange
      &&
      -- The bounty's message hash is in the DA
      daTopHash
      == applySkyDataProof proof messageHash
      &&
      -- topic ID matches
      topicId
      == triePathKey pathTopicTriePath
      &&
      -- heights are 0 (lead top top from leafs)
      triePathHeight pathTopicTriePath
      == 0
      && triePathHeight pathMessageTriePath
      == 0

validateTimeout :: POSIXTime -> Interval POSIXTime -> Bool
validateTimeout bountyDeadline txValidRange =
  bountyDeadline `before` txValidRange

-- Main validator function
clientTypedValidator ::
  ClientParams ->
  () ->
  ClientRedeemer ->
  ScriptContext ->
  Bool
clientTypedValidator ClientParams {..} () redeemer ctx =
  case redeemer of
    ClaimBounty proof ->
      validateClaimBounty
        bountyDeadline
        txValidRange
        bountyMessageHash
        bountyTopicId
        proof
        daTopHash
        && allPaidToCredential bountyClaimantPubKeyHash
    Timeout ->
      validateTimeout bountyDeadline txValidRange
        && allPaidToCredential bountyOffererPubKeyHash
  where
    -- DA Top hash stored in NFT
    daTopHash :: Hash
    daTopHash =
      bridgeNFTTopHash
        . fromJust
        $ getRefBridgeNFTDatumFromContext bountyNFTCurrencySymbol ctx
    -- Tx validity interval
    txValidRange = txInfoValidRange . scriptContextTxInfo $ ctx
    -- Bounty prize funds are sent to pre-configured address
    allPaidToCredential :: PubKeyHash -> Bool
    allPaidToCredential recipient =
      all (\o -> addressCredential (txOutAddress o) == PubKeyCredential recipient)
        $ txInfoOutputs (scriptContextTxInfo ctx)

------------------------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------------------------

{-# INLINEABLE clientUntypedValidator #-}
clientUntypedValidator :: ClientParams -> BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
clientUntypedValidator params _datum redeemer ctx =
  PlutusTx.check True

-- TODO: make sure to uncomment once monomorphized version of Trie is available
-- \$ clientTypedValidator
--   (fromByteStringIn params)
--   () -- ignore the untyped datum, it's unused
--   (fromByteStringIn $ PlutusTx.unsafeFromBuiltinData redeemer)
--   (PlutusTx.unsafeFromBuiltinData ctx)

clientValidatorScript ::
  ClientParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
clientValidatorScript params =
  $$(PlutusTx.compile [||clientUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
