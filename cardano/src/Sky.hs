{-# LANGUAGE BlockArguments #-}

module Sky (bridge, policy, client) where

-- import Data.ByteString (ByteString)

-- import Utils qualified (token)

--import Plutarch.Api.V2 (type PScriptContext)
import Plutarch.Prelude
import Plutarch.DataRepr (type PDataFields)
import Plutarch.Builtin (pforgetData)
import Plutarch.Api.V1.AssocMap (plookup)

import PlutusLedgerApi.V2 (type PubKeyHash, type TxOutRef, type CurrencySymbol)

import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2

-- TODO: Word32
-- see https://github.com/IntersectMBO/plutus/blob/master/plutus-core/plutus-core/src/PlutusCore/Data.hs#L171
type State = PInteger

--type Signature = PByteString -- ???: PPubKeyHash


-- newtype SkyDatum = SkyDatum { state :: State }
newtype SkyDatum (s :: S) = SkyDatum (Term s (PDataRecord '[ "state" ':= State ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)
instance DerivePlutusType SkyDatum where type DPTStrat _ = PlutusTypeData

{-
-- can status be updated by anyone who have a signature?
-- or the Tx itself must be signed in certain way (so there's one allowed account)
data SkyRedeemer (s :: S)
  = Check (Term s (PDataRecord '[]))
  | Update (Term s (PDataRecord
    '[ "old" ':= State -- ???: do we actually need it?
                       -- we can either keey state at contract output or at Redeemer input
     , "new" ':= State
     , "sig" ':= Signature ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType SkyRedeemer where type DPTStrat _ = PlutusTypeData
-}


-- should the bridge be unique?  It's supposed to be an unique bridge per cluster, I guess
--

-- ???: Plutarch.API.V2.PPubKeyHash
-- or we can just check an NFT topic token,
-- which issued according to baked-in rules (static yet)
bridge :: CurrencySymbol -> ClosedTerm (PAsData SkyDatum :--> PAsData State :--> PAsData PScriptContext :--> POpaque) -- PValidator
bridge _token = plam $ \_datm _redm _ctx -> popaque $ pconstant ()
  -- for Update:
  --   inputs contains
  --   outputs contains a tx to the same hashOf(script & operator key) and a new datum,
  --   new datum is of proper type (?)

  --   but isn't that done by utxo already?
  --
  -- for Check:
  --   txInfoReferenceInputs check for utxo with the state (???: should that be
  --   on-chain)
  --   Should that be even a transaction, not a pure off-chain (wallet lookup)?

{-
yieldTopic :: TxOutRef -> () -> ScriptContext -> Bool
yieldTopic oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  -- also check that (txInfoOutputs ctx) contains txOutReferenceScript of bridge
  -- with a valid txOutDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tokenName && amt == 1
        _                -> False
        where
          tokenName = "SkyProtocol Topic"
-}

tokenName :: PTokenName s
tokenName = PTokenName $ pconstant "SkyProtocol bridge policy token"

-- ???: burning cases?
-- TODO: NonEmptyList PubKeyHash -- as a first step to the multi-sign
-- TODO: NonEmptyList (NonEmptyList PubKeyHash) -- we can check several states at once
policy :: PubKeyHash -> TxOutRef -> ClosedTerm (PAsData PUnit :--> PAsData PScriptContext :--> POpaque) -- PMintingPolicy
policy key ref = plam $ \_unit ctx' -> popaque $ unTermCont do
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
  let ownSym = pfield @"_0" # mintFlds
  txInfo <- tcont $ pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
  pguardC "UTxO not consumed" $
    pany # plam (\x -> pfield @"outRef" # x #== pdata (pconstant ref)) #$ pfromData $
      getField @"inputs" txInfo
  pguardC "Wrong NFT mint amount" $
    pvalueOf # getField @"mint" txInfo # ownSym # pcon tokenName #== 1
  pure $ pconstant ()

client :: CurrencySymbol -> ClosedTerm (PAsData PUnit :--> PAsData State :--> PAsData PScriptContext :--> POpaque) -- PValidator
client token = plam $ \_unit redm ctx -> popaque $ unTermCont do
  -- TODO: proper Datum evidence (in inputs from the same script or from minting policy)
  let refs = pfromData $ pfield @"referenceInputs" #$ pfield @"txInfo" # ctx
  pguardC "No bridge policy token holder referenced" $
    pany # plam (\ref -> unTermCont do
                    let res = pfield @"resolved" # ref
                    -- check token
                    PValue currs <- tcont . pmatch $ pfield @"value" # res
                    PJust vals <- tcont . pmatch $ plookup # pconstant token # currs
                    (PJust val) <- tcont . pmatch $ plookup # pcon tokenName # vals
                    -- compare state
                    POutputDatum datum <- tcont . pmatch $ pfield @"datum" # res -- ???: ptraceError "Info: Bridge state input with inlined datum expected"
                    PDatum state <- tcont . pmatch $ pfield @"outputDatum" # datum
                    pure $ val #== pconstant 0 #&& pforgetData redm #== state -- TODO: as early fail as possible
                ) #$ refs
  pure $ pconstant ()

-- condition helper
pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s

-- Going to implement it the next way:
-- - client contract could be completely off-chain check.
-- - bridge could be a script with datum that contain just a plain state (byte32?) with several validations:
--     + output contains exactly single utxo, and it's pointed to bridge contract address
--     + datum state have a proper format
--     + current tx signed by the same operator as the tx in bridge input that came from bridge if there's such an input, or from the topic contract address otherwise
-- - topic contract would generate unique initial utxo for a bridge (per topic) with such validations:
--     + inputs contain a backed-in unique utxoRef
--     + there's a single output to the bridge
--     + datum state have a proper format
--     + spending tx contains signatures from the input tx (should be exactly one, since backed-in utxoRef is unique)


-- -- so in general, topic contract would be similar to the NFT, and bridge -- to oracle pattern.  No coins and paying (except default fees) included at the moment.

-- Design may change yet during an implementation.
