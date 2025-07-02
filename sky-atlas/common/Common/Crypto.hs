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

newtype Digest hf a = Digest {digestByteString :: FixedLengthByteString hf}
  deriving newtype (P.Eq, P.Show, ToInt, FromInt, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)
  deriving newtype (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Static intent to transform with a hash or encryption function f
newtype PlainText f a = PlainText a
  deriving stock (Generic)
  deriving newtype (HP.Show, HP.Eq)
  deriving anyclass (HasBlueprintDefinition)

data Blake2b_256 = Blake2b_256 -- static knowledge of hash function
  deriving (HP.Eq, HP.Show)

data DigestRef hf x = DigestRef {digestRefDigest :: Digest hf x, digestRefValue :: x}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

data DigestMRef hf x = DigestMRef {digestMRefDigest :: Digest hf x, digestMRefValue :: Maybe x}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type DataDigest hf = Digest hf BuiltinByteString

type Hash = Digest Blake2b_256

type DataHash = Hash BuiltinByteString

type HashRef = DigestRef Blake2b_256

-- * Classes

class (StaticLength hf) => HashFunction hf where
  hashFunction :: BuiltinByteString -> Digest hf a

class (HashFunction hf) => DigestibleRef hf r where
  getDigest :: r a -> Digest hf a
  makeDigestRef :: Digest hf a -> a -> r a

  --  | r -> hf
  digestRef_ :: (ToByteString a) => Proxy hf -> a -> r a
  digestRef_ _ a = makeDigestRef @hf (computeDigest @hf a) a
  lookupDigestRef :: (ToByteString a) => Digest hf a -> r a
  lookupDigestRef d = makeDigestRef d $ lookupDigest d

-- * Instances

-- ** Digest

instance (HashFunction hf) => LiftEq (Digest hf) where
  liftEq = (==)

instance (HashFunction hf) => LiftShow (Digest hf) where
  liftShowsPrec = showsPrec

instance (HashFunction hf) => LiftToByteString (Digest hf) where
  liftToByteString = toByteString
  liftByteStringOut = byteStringOut

instance (HashFunction hf) => LiftFromByteString (Digest hf) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance (HashFunction hf) => LiftToData (Digest hf) where
  liftToBuiltinData = toBuiltinData

instance (HashFunction hf) => LiftFromData (Digest hf) where
  liftFromBuiltinData = fromBuiltinData

instance (HashFunction hf) => LiftUnsafeFromData (Digest hf) where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData

-- ** Blake2b_256

instance HashFunction Blake2b_256 where
  hashFunction = Digest . FixedLengthByteString . blake2b_256

instance StaticLength Blake2b_256 where
  staticLength = const 32

instance P.Eq Blake2b_256 where
  Blake2b_256 == Blake2b_256 = True

instance P.Show Blake2b_256 where
  show Blake2b_256 = "Blake2b_256"

-- ** DigestRef

instance (HashFunction hf, P.Show a) => P.Show (DigestRef hf a) where
  showsPrec prec (DigestRef d x) = showApp prec "DigestRef" [showArg d, showArg x]

-- instance (HashFunction hf, HP.Show a) => HP.Show (DigestRef hf a) where
--   showsPrec prec (DigestRef _ x) = HP.showApp prec "DigestRef" [HP.showArg d, HP.showArg x]

instance (HashFunction hf) => HP.Eq (DigestRef hf x) where
  DigestRef ah _ == DigestRef bh _ = ah == bh

instance (HashFunction hf) => P.Eq (DigestRef hf x) where
  DigestRef ah _ == DigestRef bh _ = ah == bh

instance (HashFunction hf) => DigestibleRef hf (DigestRef hf) where
  getDigest = digestRefDigest
  makeDigestRef d a = DigestRef d a

instance (HashFunction hf) => ToByteString (DigestRef hf x) where
  toByteString = toByteString . digestRefDigest
  byteStringOut = byteStringOut . digestRefDigest

instance (HashFunction hf, ToByteString x) => FromByteString (DigestRef hf x) where
  fromByteString = lookupDigestRef @hf . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> lookupDigestRef @hf

instance (HashFunction hf, Dato a, Monad e) => PreWrapping e (DigestRef hf) a where
  wrap = return . digestRef_ (Proxy @hf)

instance (HashFunction hf, Dato a, Monad e) => Wrapping e (DigestRef hf) a where
  unwrap = return . digestRefValue

instance (HashFunction hf) => LiftShow (DigestRef hf) where
  liftShowsPrec = showsPrec

instance (HashFunction hf) => LiftToByteString (DigestRef hf) where
  liftToByteString = toByteString . digestRefDigest
  liftByteStringOut = byteStringOut . digestRefDigest

instance (HashFunction hf) => LiftFromByteString (DigestRef hf) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance (HashFunction hf) => LiftToData (DigestRef hf) where
  liftToBuiltinData = toBuiltinData . digestRefDigest

instance (HashFunction hf) => LiftFromData (DigestRef hf) where
  liftFromBuiltinData b = fromBuiltinData b >>= return . \d -> DigestRef d $ lookupDigest d

instance (HashFunction hf) => LiftUnsafeFromData (DigestRef hf) where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData -. \d -> DigestRef d $ lookupDigest d

instance (HashFunction hf) => LiftEq (DigestRef hf) where
  liftEq = (==)

instance (HashFunction hf, Monad e) => LiftPreWrapping e (DigestRef hf) where
  liftWrap = wrap

instance (HashFunction hf, Monad e) => LiftWrapping e (DigestRef hf) where
  liftUnwrap = unwrap

-- instance (HashFunction hf, MonadReader r m) => LiftWrapping m (DigestRef hf) where
--   liftUnwrap = unwrap

{-
-- ** DigestMRef

instance (HashFunction hf, P.Show a) => P.Show (DigestMRef hf a) where
  showsPrec prec (DigestMRef d x) = showApp prec "DigestMRef" [showArg d, showArg x]

instance (HashFunction hf) => P.Eq (DigestMRef hf x) where
  DigestMRef ah _ == DigestMRef bh _ = ah == bh

instance (HashFunction hf) => HP.Eq (DigestMRef hf x) where
  (==) = (P.==)

instance (HashFunction hf) => DigestibleRef hf (DigestMRef hf) where
  getDigest = digestMRefDigest
  makeDigestRef d a = DigestMRef d (Just a)

instance (HashFunction hf) => ToByteString (DigestMRef hf x) where
  toByteString = toByteString . digestMRefDigest
  byteStringOut = byteStringOut . digestMRefDigest

instance (HashFunction hf, ToByteString x, FromByteString x) => FromByteString (DigestMRef hf x) where
  fromByteString = lookupDigestRef . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> lookupDigestRef

instance (HashFunction hf, Dato a) => PreWrapping Identity (DigestMRef hf) a where
  wrap = return . digestRef

-- Unsafe. TODO: Implement a safer version with an error monad
instance (HashFunction hf, Dato a) => Wrapping Identity (DigestMRef hf) a where
  unwrap = return . fromJust . digestMRefValue

instance (HashFunction hf) => LiftShow (DigestMRef hf) where
  liftShowsPrec = showsPrec

instance (HashFunction hf) => LiftToByteString (DigestMRef hf) where
  liftToByteString = toByteString . digestMRefDigest
  liftByteStringOut = byteStringOut . digestMRefDigest

instance (HashFunction hf) => LiftFromByteString (DigestMRef hf) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance (HashFunction hf) => LiftToData (DigestMRef hf) where
  liftToBuiltinData = toBuiltinData . digestMRefDigest

instance (HashFunction hf) => LiftFromData (DigestMRef hf) where
  liftFromBuiltinData b = fromBuiltinData b >>= return . \d -> DigestMRef d . Just $ lookupDigest d

instance (HashFunction hf) => LiftUnsafeFromData (DigestMRef hf) where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData -. \d -> DigestMRef d . Just $ lookupDigest d

instance (HashFunction hf) => LiftEq (DigestMRef hf) where
  liftEq = (==)

instance (HashFunction hf) => LiftPreWrapping Identity (DigestMRef hf) where
  liftWrap = wrap

instance (HashFunction hf) => LiftWrapping Identity (DigestMRef hf) where
  liftUnwrap = unwrap

-- instance (HashFunction hf, MonadReader r m) => LiftWrapping m (DigestMRef hf) where
--   liftUnwrap = unwrap
-}

-- ** LiftRef

instance (HashFunction hf, DigestibleRef hf r) => DigestibleRef hf (LiftRef r) where
  -- instance (HashFunction hf, DigestibleRef hf (r hf)) => DigestibleRef hf (LiftRef (r hf)) where
  getDigest = getDigest . liftref
  makeDigestRef d a = LiftRef $ makeDigestRef d a

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

-- ** PlainText

instance (Show a) => Show (PlainText f a) where
  showsPrec prec (PlainText a) = showApp prec "PlainText" [showArg a]

instance (Eq a) => Eq (PlainText f a) where
  PlainText a == PlainText b = a == b

-- TODO: Dato preservation + inputs

-- * Helpers

-- ** Digests

digest :: (HashFunction hf, ToByteString a) => PlainText hf a -> Digest hf a
digest (PlainText m :: PlainText hf a) = computeDigest m

computeDigest :: (HashFunction hf, ToByteString a) => a -> Digest hf a
computeDigest a = hashFunction (toByteString a)

digestRef :: (HashFunction hf, ToByteString a) => a -> DigestRef hf a
digestRef x = DigestRef (computeDigest x) x

-- Make that a monadic method on a typeclass
lookupDigest :: (HashFunction hf) => Digest hf a -> a
lookupDigest = traceError "Cannot get a value from its digest"

-- lookupDigestRef :: (HashFunction hf) => Digest hf a -> DigestRef hf a
-- lookupDigestRef d = DigestRef d $ lookupDigest d

castDigest :: Digest hf a -> Digest hf b
castDigest (Digest x) = Digest x

computeHash :: (ToByteString a) => a -> DataHash
computeHash = computeDigest . toByteString

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
