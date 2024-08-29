module Conversions (pconvert) where

import Plutarch (S, Term)
import Plutarch.Prelude (PType)
import Plutarch.TryFrom (PTryFrom (..))
import Plutarch.Unsafe (punsafeCoerce)

pconvert :: forall (b :: PType) (a :: PType) (s :: S). (PTryFrom a b) => Term s a -> Term s b
pconvert = punsafeCoerce
