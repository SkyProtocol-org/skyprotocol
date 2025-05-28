{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Crypto where

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN, SignKeyDSIGN (..))
import Common.Types
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Prelude
import PlutusTx.Show

-- * Types

-- A pair (pubKey, signature) of signature by a single authority
newtype SingleSig = SingleSig {getSingleSig :: (PubKey, Signature)}
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via (Bytes32, Bytes64)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Signatures produced by data operators for top hash
newtype MultiSig = MultiSig [SingleSig]
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- List of pubkeys that must sign and minimum number of them that must sign
newtype MultiSigPubKey = MultiSigPubKey {getMultiSigPubKey :: ([PubKey], UInt16)}
  -- data MultiSigPubKey = MultiSigPubKey { multiSigPubKeyKeys :: [PubKey], multiSigPubKeyThreshold :: UInt16 }
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via ([PubKey], UInt16)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type DataDigest hf = Digest hf BuiltinByteString

type Hash = Digest Blake2b_256

type DataHash = Hash BuiltinByteString

type HashRef = DigestRef Blake2b_256

newtype PubKey = PubKey {getPubKey :: Bytes32}
  deriving newtype (Show)
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via Bytes32
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Don't use that on-chain! At least not without say much homomorphic encryption.
-- TODO: move that to a separate file, too, that is incompatible with on-chain
newtype SecKey = SecKey {getSecKey :: Bytes32}
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via Bytes32
  deriving newtype (Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Signature = Signature {getSignature :: Bytes64}
  deriving newtype (Show)
  deriving (Eq, ToByteString, FromByteString) via Bytes64
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Digest hf a = Digest {digestByteString :: FixedLengthByteString hf}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
  deriving (Eq, ToInt, FromInt, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via FixedLengthByteString hf

-- Static intent to transform with a hash or encryption function f
newtype PlainText f a = PlainText a
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

data Blake2b_256 = Blake2b_256 -- static knowledge of hash function

data DigestRef hf x = DigestRef {digestRefDigest :: Digest hf x, digestRefValue :: x}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- * Classes

class
  (StaticLength hf) =>
  HashFunction hf
  where
  hashFunction :: BuiltinByteString -> Digest hf a

class
  (HashFunction hf) =>
  DigestibleRef hf r
  where
  getDigest :: r a -> Digest hf a

-- * Instances

-- ** SingleSig

instance Show SingleSig where
  showsPrec prec (SingleSig ps) = showApp prec "SingleSig" [showArg ps]

-- ** MultiSigPubKey

instance Show MultiSigPubKey where
  showsPrec prec (MultiSigPubKey ln) = showApp prec "MultiSigPubKey" [showArg ln]

instance Dato MultiSigPubKey

-- ** Digest

instance
  (HashFunction hf) =>
  Show (Digest hf a)
  where
  show (Digest x) = "Digest " <> show x

instance
  (HashFunction hf) =>
  Dato (Digest hf a)

instance
  (HashFunction hf) =>
  LiftEq (Digest hf)
  where
  liftEq = (==)

instance
  (HashFunction hf) =>
  LiftShow (Digest hf)
  where
  liftShowsPrec = showsPrec

instance
  (HashFunction hf) =>
  LiftToByteString (Digest hf)
  where
  liftToByteString = toByteString
  liftByteStringOut = byteStringOut

instance
  (HashFunction hf) =>
  LiftFromByteString (Digest hf)
  where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance
  (HashFunction hf) =>
  LiftToData (Digest hf)
  where
  liftToBuiltinData = toBuiltinData

instance
  (HashFunction hf) =>
  LiftFromData (Digest hf)
  where
  liftFromBuiltinData = fromBuiltinData

instance
  (HashFunction hf) =>
  LiftUnsafeFromData (Digest hf)
  where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData

instance
  (HashFunction hf) =>
  LiftDato (Digest hf)

-- ** Blake2b_256

instance HashFunction Blake2b_256 where
  hashFunction = Digest . FixedLengthByteString . blake2b_256

instance StaticLength Blake2b_256 where
  staticLength = const 32

-- ** DigestRef

instance (HashFunction hf) => DigestibleRef hf (DigestRef hf) where
  getDigest = digestRefDigest

instance
  (HashFunction hf) =>
  Eq (DigestRef hf x)
  where
  DigestRef ah _ == DigestRef bh _ = ah == bh

instance (HashFunction hf) => ToByteString (DigestRef hf x) where
  toByteString = toByteString . digestRefDigest
  byteStringOut = byteStringOut . digestRefDigest

instance (HashFunction hf) => FromByteString (DigestRef hf x) where
  fromByteString = lookupDigestRef . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> lookupDigestRef

instance (HashFunction hf, Show a) => Show (DigestRef hf a) where
  showsPrec prec (DigestRef _ x) = showApp prec "digestRef" [showArg x]

instance (HashFunction hf, Dato a) => PreWrapping Identity (DigestRef hf) a where
  wrap = return . digestRef

instance (HashFunction hf, Dato a) => Wrapping Identity (DigestRef hf) a where
  unwrap = return . digestRefValue

instance
  (HashFunction hf) =>
  LiftShow (DigestRef hf)
  where
  liftShowsPrec = showsPrec

instance
  (HashFunction hf) =>
  LiftToByteString (DigestRef hf)
  where
  liftToByteString = toByteString . digestRefDigest
  liftByteStringOut = byteStringOut . digestRefDigest

instance
  (HashFunction hf) =>
  LiftFromByteString (DigestRef hf)
  where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance
  (HashFunction hf) =>
  LiftToData (DigestRef hf)
  where
  liftToBuiltinData = toBuiltinData . digestRefDigest

instance
  (HashFunction hf) =>
  LiftFromData (DigestRef hf)
  where
  liftFromBuiltinData b = fromBuiltinData b >>= return . \d -> DigestRef d $ lookupDigest d

instance
  (HashFunction hf) =>
  LiftUnsafeFromData (DigestRef hf)
  where
  liftUnsafeFromBuiltinData = unsafeFromBuiltinData -. \d -> DigestRef d $ lookupDigest d

instance
  (HashFunction hf) =>
  LiftEq (DigestRef hf)
  where
  liftEq = (==)

instance
  (HashFunction hf) =>
  LiftDato (DigestRef hf)

instance
  (HashFunction hf) =>
  LiftPreWrapping Identity (DigestRef hf)
  where
  liftWrap = wrap

instance
  (HashFunction hf) =>
  LiftWrapping Identity (DigestRef hf)
  where
  liftUnwrap = unwrap

-- ** LiftRef

instance
  (HashFunction hf, DigestibleRef hf r) =>
  DigestibleRef hf (LiftRef r)
  where
  getDigest = getDigest . liftref

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

lookupDigest :: (HashFunction hf) => Digest hf a -> a
lookupDigest = traceError "Cannot get a value from its digest"

lookupDigestRef :: (HashFunction hf) => Digest hf a -> DigestRef hf a
lookupDigestRef d = DigestRef d $ lookupDigest d

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

-- * Meta declarations

{-
PlutusTx.unstableMakeIsData ''Blake2b_256
PlutusTx.unstableMakeIsData ''Digest
PlutusTx.makeIsDataSchemaIndexed ''FixedLengthByteString [('FixedLengthByteString, 0)]
PlutusTx.makeLift ''Digest
PlutusTx.makeIsDataSchemaIndexed ''Digest [('Digest, 0)]

PlutusTx.makeIsDataSchemaIndexed ''Digest [('Digest, 0)]
PlutusTx.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''SingleSig [('SingleSig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSig [('MultiSig, 0)]
PlutusTx.makeLift ''DataHash
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''SingleSig
PlutusTx.makeLift ''MultiSig
-}
