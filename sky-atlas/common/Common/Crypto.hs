{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Crypto where

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN, SignKeyDSIGN (..))
import Common.Types
import Control.Monad (Monad)
import Data.Aeson
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusTx as P
import PlutusTx.Blueprint as P
import PlutusTx.Builtins as P
import PlutusTx.Prelude as P
import PlutusTx.Show as P
import Prelude qualified as HP

-- * Types

-- A pair (pubKey, signature) of signature by a single authority
newtype SingleSig = SingleSig {getSingleSig :: (PubKey, Signature)}
  deriving newtype (P.Eq, P.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Signatures produced by data operators for top hash
newtype MultiSig = MultiSig [SingleSig]
  deriving newtype (P.Eq, P.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- List of pubkeys that must sign and minimum number of them that must sign
newtype MultiSigPubKey = MultiSigPubKey {getMultiSigPubKey :: ([PubKey], UInt16)}
  deriving newtype (P.Eq, P.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype PubKey = PubKey {getPubKey :: Bytes32}
  deriving newtype (P.Show, P.Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Show, HP.Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Don't use that on-chain! At least not without say much homomorphic encryption.
-- TODO: move that to a separate file, too, that is incompatible with on-chain
newtype SecKey = SecKey {getSecKey :: Bytes32}
  deriving newtype (P.Eq, P.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Show, HP.Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Signature = Signature {getSignature :: Bytes64}
  deriving newtype (P.Show, P.Eq, ToByteString, FromByteString, UnsafeFromData, ToData, FromData)
  deriving newtype (HP.Show, HP.Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Digest d a = Digest {getDigest :: d}
  deriving newtype (P.Eq, P.Show, ToInt, FromInt, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Blake2b_256 = Blake2b_256 {getBlake2b_256 :: Bytes32}
  deriving newtype (HP.Eq, HP.Show)
  deriving newtype (P.Eq, P.Show, ToInt, FromInt, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type Hash = Blake2b_256

data HashRef d x = HashRef {hashRefHash :: d, hashRefValue :: x}
  deriving (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

data HashMRef d x = HashMRef {hashMRefHash :: d, hashMRefValue :: Maybe x}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- * Classes
class (Dato d) => IsHash d where
  hashFunction :: BBS -> d

class (IsHash d) => DigestibleRef d r | r -> d where
  refDigest :: r a -> d
  makeHashRef :: d -> a -> r a

  --  | r -> d
  digestRef_ :: (ToByteString a) => a -> r a
  digestRef_ a = makeHashRef (computeDigest @d a) a
  lookupHashRef :: (ToByteString a) => d -> r a
  lookupHashRef d = makeHashRef d $ lookupDigest d

-- * Instances

-- ** Digest

instance (IsHash d) => LiftEq (Digest d) where
  liftEq = (==)

instance (IsHash d) => LiftShow (Digest d) where
  liftShowsPrec = showsPrec

instance (IsHash d) => LiftToByteString (Digest d) where
  liftToByteString = toByteString
  liftByteStringOut = byteStringOut

instance (IsHash d) => LiftFromByteString (Digest d) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance (IsHash d) => LiftToData (Digest d) where
  liftToBuiltinData = toBuiltinData

instance (IsHash d) => LiftFromData (Digest d) where
  liftFromBuiltinData = fromBuiltinData

instance (IsHash d) => LiftUnsafeFromData (Digest d) where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData

-- ** Blake2b_256

instance IsHash Blake2b_256 where
  hashFunction = Blake2b_256 . Bytes32 . blake2b_256

-- ** HashRef

instance (P.Eq d, P.Eq x) => P.Eq (HashRef d x) where
  HashRef ah _ == HashRef bh _ = ah == bh

instance (P.Show d, P.Show a) => P.Show (HashRef d a) where
  showsPrec prec (HashRef d x) = showApp prec "HashRef" [showArg d, showArg x]

instance (IsHash d) => DigestibleRef d (HashRef d) where
  refDigest = hashRefHash
  makeHashRef d a = HashRef d a

instance (ToByteString d) => ToByteString (HashRef d x) where
  toByteString = toByteString . hashRefHash
  byteStringOut = byteStringOut . hashRefHash

-- fake for the sake of Dato
instance (Dato d, ToByteString x) => FromByteString (HashRef d x) where
  fromByteString = fromJust Nothing -- XXX lookupHashRef @d . fromByteString
  byteStringIn _ = fromJust Nothing -- XXX byteStringIn isTerminal <&> lookupHashRef @d

instance (IsHash d, Dato a, Monad e) => PreWrapping e (HashRef d) a where
  wrap = return . digestRef_

instance (IsHash d, Dato a, Monad e) => Wrapping e (HashRef d) a where
  unwrap = return . hashRefValue

instance (IsHash d) => LiftShow (HashRef d) where
  liftShowsPrec = showsPrec

instance (IsHash d) => LiftToByteString (HashRef d) where
  liftToByteString = toByteString . hashRefHash
  liftByteStringOut = byteStringOut . hashRefHash

instance (IsHash d) => LiftFromByteString (HashRef d) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance (IsHash d) => LiftToData (HashRef d) where
  liftToBuiltinData = toBuiltinData . hashRefHash

instance (IsHash d) => LiftFromData (HashRef d) where
  liftFromBuiltinData b = fromBuiltinData b >>= return . \d -> HashRef d $ lookupDigest d

instance (IsHash d) => LiftUnsafeFromData (HashRef d) where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData -. \d -> HashRef d $ lookupDigest d

instance (IsHash d) => LiftEq (HashRef d) where
  liftEq = (==)

instance (IsHash d, Monad e) => LiftPreWrapping e (HashRef d) where
  liftWrap = wrap

instance (IsHash d, Monad e) => LiftWrapping e (HashRef d) where
  liftUnwrap = unwrap

-- instance (IsHash d, MonadReader r m) => LiftWrapping m (HashRef d) where
--   liftUnwrap = unwrap

{-
-- ** HashMRef

instance (IsHash d, P.Show a) => P.Show (HashMRef d a) where
  showsPrec prec (HashMRef d x) = showApp prec "HashMRef" [showArg d, showArg x]

instance (IsHash d) => P.Eq (HashMRef d x) where
  HashMRef ah _ == HashMRef bh _ = ah == bh

instance (IsHash d) => HP.Eq (HashMRef d x) where
  (==) = (P.==)

instance (IsHash d) => DigestibleRef d (HashMRef d) where
  refDigest = hashMRefHash
  makeHashRef d a = HashMRef d (Just a)

instance (IsHash d) => ToByteString (HashMRef d x) where
  toByteString = toByteString . hashMRefHash
  byteStringOut = byteStringOut . hashMRefHash

instance (IsHash d, ToByteString x, FromByteString x) => FromByteString (HashMRef d x) where
  fromByteString = lookupHashRef . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> lookupHashRef

instance (IsHash d, Dato a) => PreWrapping Identity (HashMRef d) a where
  wrap = return . digestRef

-- Unsafe. TODO: Implement a safer version with an error monad
instance (IsHash d, Dato a) => Wrapping Identity (HashMRef d) a where
  unwrap = return . fromJust . hashMRefValue

instance (IsHash d) => LiftShow (HashMRef d) where
  liftShowsPrec = showsPrec

instance (IsHash d) => LiftToByteString (HashMRef d) where
  liftToByteString = toByteString . hashMRefHash
  liftByteStringOut = byteStringOut . hashMRefHash

instance (IsHash d) => LiftFromByteString (HashMRef d) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance (IsHash d) => LiftToData (HashMRef d) where
  liftToBuiltinData = toBuiltinData . hashMRefHash

instance (IsHash d) => LiftFromData (HashMRef d) where
  liftFromBuiltinData b = fromBuiltinData b >>= return . \d -> HashMRef d . Just $ lookupDigest d

instance (IsHash d) => LiftUnsafeFromData (HashMRef d) where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData -. \d -> HashMRef d . Just $ lookupDigest d

instance (IsHash d) => LiftEq (HashMRef d) where
  liftEq = (==)

instance (IsHash d) => LiftPreWrapping Identity (HashMRef d) where
  liftWrap = wrap

instance (IsHash d) => LiftWrapping Identity (HashMRef d) where
  liftUnwrap = unwrap

-- instance (IsHash d, MonadReader r m) => LiftWrapping m (HashMRef d) where
--   liftUnwrap = unwrap
-}

-- ** LiftRef

instance (IsHash d) => DigestibleRef d (LiftRef (HashRef d)) where
  refDigest = refDigest . liftref
  makeHashRef d a = LiftRef $ makeHashRef d a

instance (IsHash d) => DigestibleRef d (LiftRef (Digest d)) where
  refDigest = getDigest . liftref
  makeHashRef d _ = LiftRef . Digest $ d

-- ** SecKey

instance FromJSON SecKey where
  parseJSON v = do
    k <- parseJSON v
    return $ SecKey $ ofHex k

-- ** PubKeyHash

-- TODO implement isomorphism between builtin PubKeyHash and our (Digest Blake2b_256 PubKey)

instance ToByteString PubKeyHash where
  toByteString = getPubKeyHash
  byteStringOut = byteStringOut . getPubKeyHash

instance FromByteString PubKeyHash where
  --  fromByteString = PubKeyHash
  byteStringIn isTerminal = byteStringIn isTerminal <&> PubKeyHash

-- TODO: Dato preservation + inputs

-- * Helpers

-- ** Digests

computeDigest :: (IsHash d, ToByteString a) => a -> d
computeDigest = hashFunction . toByteString

digestRef :: (IsHash d, ToByteString a) => a -> HashRef d a
digestRef a = HashRef (computeDigest a) a

-- Make that a monadic method on a typeclass with a cache/db for lookup
lookupDigest :: (IsHash d) => d -> a
lookupDigest = fromJust Nothing -- XXX traceError "Cannot get a value from its digest"

-- lookupHashRef :: (IsHash d) => d -> HashRef d a
-- lookupHashRef d = HashRef d $ lookupDigest d

-- ** Ed25519 Signatures

-- verify with PlutusTx.Builtins.verifyEd25519Signature
-- TODO handle bad key

-- | Sign message with given secret key.
signMessage :: (ToByteString a) => SecKey -> a -> Signature
signMessage sk msg =
  fromByteString
    . toBuiltin
    . DSIGN.rawSerialiseSigDSIGN
    $ DSIGN.signDSIGN
      ()
      (fromBuiltin . toByteString $ msg)
      (fromJust (DSIGN.rawDeserialiseSignKeyDSIGN . fromBuiltin . toByteString $ sk) :: SignKeyDSIGN Ed25519DSIGN)

-- | Derive a PK from an SK -- TODO use Maybe
derivePubKey :: SecKey -> PubKey
derivePubKey = fromByteString . toBuiltin . DSIGN.rawSerialiseVerKeyDSIGN . DSIGN.deriveVerKeyDSIGN @Ed25519DSIGN . fromJust . DSIGN.rawDeserialiseSignKeyDSIGN . fromBuiltin . toByteString

verifySignature :: (ToByteString a) => PubKey -> a -> Signature -> Bool
verifySignature pubKey message sig =
  verifyEd25519Signature (toByteString pubKey) (toByteString message) (toByteString sig)

-- Function that checks if a SingleSig is valid
singleSigValid :: (ToByteString a) => a -> SingleSig -> Bool
singleSigValid message (SingleSig (pubKey, sig)) =
  verifySignature pubKey message sig

-- WARNING: 'nub' and 'elem' use GHC Prelude Eq
-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: (ToByteString a) => MultiSigPubKey -> a -> MultiSig -> Bool
multiSigValid (MultiSigPubKey (pubKeys, minSigs)) message (MultiSig singleSigs) =
  let -- Extract the public keys from the SingleSig values
      pubKeysInSignatures = fmap (\(SingleSig (pubKey, _)) -> pubKey) singleSigs
      -- Check for duplicates by comparing the list to its nub version
      noDuplicates = pubKeysInSignatures == nub pubKeysInSignatures
   in if not noDuplicates
        then False -- Duplicates found, return False
        else
          let -- Filter for valid signatures from required public keys
              validSignatures = filter (\ss@(SingleSig (pubKey, _)) -> pubKey `elem` pubKeys && singleSigValid message ss) singleSigs
           in length validSignatures >= toInt minSigs

-- ** Template Haskell declarations

P.makeLift ''SecKey

P.makeLift ''PubKey

P.makeLift ''Signature

P.makeLift ''SingleSig

P.makeLift ''MultiSigPubKey

P.makeLift ''MultiSig

P.makeLift ''Blake2b_256
