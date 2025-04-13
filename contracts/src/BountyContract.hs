{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module BountyContract where

import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1
  ( Credential (PubKeyCredential),
    Lovelace,
    POSIXTime,
    PubKeyHash (..),
    addressCredential,
  )
import PlutusLedgerApi.V1.Interval (Interval, after, before, contains)
import PlutusLedgerApi.V1.Value
  ( AssetClass (..),
    assetClassValueOf,
    flattenValue,
    lovelaceValueOf,
    valueOf,
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
    Value (..),
    from,
    to,
    txInInfoResolved,
    txOutDatum,
  )
import PlutusLedgerApi.V2.Contexts (findDatum, getContinuingOutputs, scriptContextTxInfo, txInfoValidRange)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
  ( BuiltinByteString,
    appendByteString,
    blake2b_256,
    consByteString,
    emptyByteString,
    equalsByteString,
    lessThanInteger,
    readBit,
    sha2_256,
    verifyEd25519Signature,
  )
import PlutusTx.List (all)
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import SkyBase
import SkyBridgeContract (BridgeNFTDatum (..), getRefBridgeNFTDatumFromContext)
import SkyCrypto
import SkyDA
import Trie

------------------------------------------------------------------------------
-- Initialization parameters for client contract
------------------------------------------------------------------------------

type ClientParams = BuiltinByteString

data DecodedClientParams = DecodedClientParams
  { -- | Unique currency symbol (hash of minting policy) of the bridge contract NFT
    bountyNFTCurrencySymbol :: CurrencySymbol,
    -- | Credential of claimant (bounty prize can only be sent to this credential)
    bountyClaimantPubKeyHash :: PubKeyHash,
    -- | Credential of offerer (to whom to send back the bounty if not claimed before timeout)
    bountyOffererPubKeyHash :: PubKeyHash,
    -- | ID of topic in which data must be published
    bountyTopicId :: TopicId,
    -- | Hash of data that must be proven to be present in DA
    bountyMessageHash :: DataHash,
    bountyDeadline :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

getDecodedClientParams :: DecodedClientParams -> (CurrencySymbol, PubKeyHash, PubKeyHash, TopicId, DataHash, POSIXTime)
getDecodedClientParams (DecodedClientParams a b c d e f) = (a, b, c, d, e, f)

instance FromByteString DecodedClientParams where
  byteStringIn isTerminal = byteStringIn isTerminal <&> uncurry6 DecodedClientParams

instance ToByteString DecodedClientParams where
  byteStringOut = byteStringOut . getDecodedClientParams

-- PlutusTx.makeLift ''ClientParams
-- PlutusTx.makeIsDataSchemaIndexed ''ClientParams [('ClientParams, 0)]

------------------------------------------------------------------------------
-- Redeemers for client contract
------------------------------------------------------------------------------

data ClientRedeemer = ClaimBounty (SkyDataProof Blake2b_256) | Timeout

instance FromByteString ClientRedeemer where
  byteStringIn isTerminal =
    byteStringIn isTerminal <&> \case
      Left proof -> ClaimBounty proof
      Right () -> Timeout

-- PlutusTx.makeLift ''ClientRedeemer
-- PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('ClaimBounty, 0)]
-- PlutusTx.makeIsDataSchemaIndexed ''ClientRedeemer [('Timeout, 1)]

------------------------------------------------------------------------------
-- Client contract validator
------------------------------------------------------------------------------

-- Separating validation logic to make for easy testing
validateClaimBounty :: POSIXTime -> Interval POSIXTime -> DataHash -> TopicId -> SkyDataProof Blake2b_256 -> DataHash -> Bool
validateClaimBounty
  bountyDeadline
  txValidRange
  messageHash
  topicId
  proof@SkyDataPath {..}
  daTopHash =
    -- Check if the current slot is within the deadline
    bountyDeadline
      `after` txValidRange
      &&
      -- The bounty's message hash is in the DA
      daTopHash
      == applySkyDataProof proof messageHash
      &&
      -- topic ID matches
      topicId
      == triePathKey pathTopicTriePath
      &&
      -- heights are 0
      triePathHeight pathTopicTriePath
      == 0
      && triePathHeight pathMessageTriePath
      == 0

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
    daTopHash :: DataHash
    daTopHash = case getRefBridgeNFTDatumFromContext bountyNFTCurrencySymbol ctx of
      Nothing -> PlutusTx.traceError "bridge NFT not found"
      Just (BridgeNFTDatum topHash) -> topHash
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
