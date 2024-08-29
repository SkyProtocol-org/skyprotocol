module Sky (validator) where

import Data.ByteString (ByteString)

import Plutarch.Api.V2 (type PScriptContext)
import Plutarch.Prelude
import Plutarch.DataRepr (type PDataFields)

-- TODO: Word32
-- see https://github.com/IntersectMBO/plutus/blob/master/plutus-core/plutus-core/src/PlutusCore/Data.hs#L171
type State = PInteger

type Signature = PByteString -- TODO: what actually it is?
type PublicKey = ByteString -- TODO: what exact type?


-- newtype SkyDatum = SkyDatum { state :: State }
newtype SkyDatum (s :: S) = SkyDatum (Term s (PDataRecord '[ "state" ':= State ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)
instance DerivePlutusType SkyDatum where type DPTStrat _ = PlutusTypeData

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

validator :: PublicKey -> ClosedTerm (PAsData SkyDatum :--> PAsData SkyRedeemer :--> PAsData PScriptContext :--> POpaque)
validator _key = plam $ \_state _ _ -> popaque $ pconstant ()
