{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Contract.Bounty where

import Common
import Contract.Bridge (BridgeNFTDatum (..), getRefBridgeNFTDatumFromContext)
import Contract.DaH
import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (before, contains)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.List
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx
import Prelude qualified as HP

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

data ClientRedeemer = ClaimBounty SkyDataProofH | Timeout
  deriving (HP.Eq, HP.Show)

PlutusTx.makeLift ''ClientRedeemer
PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0), ('Timeout, 1)]

------------------------------------------------------------------------------
-- Client contract validator
------------------------------------------------------------------------------

-- Separating validation logic to make for easy testing
validateClaimBounty :: POSIXTime -> Interval POSIXTime -> Hash -> TopicId -> SkyDataProofH -> Hash -> Bool
validateClaimBounty bountyDeadline txValidRange messageHash topicId proof@SkyDataProofH {..} daTopHash =
  and
    [ -- Check if the current slot is within the deadline
      traceBool
        "Bounty deadline in the valid range"
        "Bounty deadline is not in the valid range"
        ( to bountyDeadline
            `contains` txValidRange
        ),
      -- The bounty's message hash is in the DA
      traceBool
        "message hash is in the DA"
        "message hash is not in the DA"
        ( daTopHash
            == applySkyDataProofH proof messageHash
        ),
      -- topic ID matches
      traceBool
        "TopicId matches"
        "TopicId doesn't match"
        ( topicId
            == triePathKey proofTopicTriePathH
        ),
      -- heights are 0 (lead top top from leafs)
      traceBool
        "Topic trie path is 0"
        "Topic trie path is not 0"
        ( triePathHeight proofTopicTriePathH
            == 0
        ),
      traceBool
        "Message trie path is 0"
        "Message trie path is not 0"
        ( triePathHeight proofMessageTriePathH
            == 0
        )
    ]

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
        && traceBool
          "All paid to credential"
          "Not all paid to credential"
          (allPaidToCredential bountyClaimantPubKeyHash)
    Timeout ->
      validateTimeout bountyDeadline txValidRange
        && allPaidToCredential bountyOffererPubKeyHash
  where
    bridgeNftDatum = case getRefBridgeNFTDatumFromContext bountyNFTCurrencySymbol ctx of
      Just d -> d
      Nothing -> traceError "Can't get nft datum from context"
    -- DA Top hash stored in NFT
    daTopHash :: Hash
    daTopHash = bridgeNFTTopHash bridgeNftDatum
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
clientUntypedValidator :: ClientParams -> BuiltinData -> BuiltinUnit
clientUntypedValidator params ctx =
  PlutusTx.check
    $ clientTypedValidator
      params
      () -- ignore the untyped datum, it's unused
      redeemer
      scriptContext
  where
    scriptContext = PlutusTx.unsafeFromBuiltinData ctx
    redeemer = PlutusTx.unsafeFromBuiltinData $ getRedeemer $ scriptContextRedeemer scriptContext

clientValidatorScript ::
  ClientParams ->
  CompiledCode (BuiltinData -> BuiltinUnit)
clientValidatorScript params =
  $$(PlutusTx.compile [||clientUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef params
