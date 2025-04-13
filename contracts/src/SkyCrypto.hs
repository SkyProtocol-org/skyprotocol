{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module SkyCrypto where

import Data.Functor.Identity (Identity (..))
-- hiding (Applicative, Functor, fmap, pure, (<*>))

import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH hiding (newName)
import PlutusCore.Default
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Lift
import PlutusTx.List (elem, filter, length, nub)
import PlutusTx.Prelude
import PlutusTx.Show
import PlutusTx.Utils
import SkyBase

-- * Types

-- A pair (pubKey, signature) of signature by a single authority
newtype SingleSig = SingleSig {getSingleSig :: (PubKey, Bytes64)}
  deriving (Eq, ToByteString, FromByteString) via (Bytes64, PubKey)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Signatures produced by data operators for top hash
newtype MultiSig = MultiSig [SingleSig]
  deriving (Eq, ToByteString, FromByteString) via [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- List of pubkeys that must sign and minimum number of them that must sign
newtype MultiSigPubKey = MultiSigPubKey {getMultiSigPubKey :: ([PubKey], UInt16)}
  -- data MultiSigPubKey = MultiSigPubKey { multiSigPubKeyKeys :: [PubKey], multiSigPubKeyThreshold :: UInt16 }
  deriving (Eq, ToByteString, FromByteString) via ([PubKey], UInt16)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type DataDigest hf = Digest hf BuiltinByteString

type Hash = Digest Blake2b_256

type DataHash = Hash BuiltinByteString

type HashRef = DigestRef Blake2b_256

newtype PubKey = PubKey {getPubKey :: Bytes32}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Digest hf a = Digest {digestByteString :: FixedLengthByteString hf}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Static intent to transform with a hash or encryption function f
newtype PlainText f a = PlainText a
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | Hash function type
data Blake2b_256
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

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

-- WARNING this was copied from plutus instance for haskell's 'Void' data type
-- I do not know if it can be used
instance ToData Blake2b_256 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData Blake2b_256 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData Blake2b_256 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L2 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L2 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L2 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L3 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L3 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L3 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L4 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L4 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L4 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L5 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L5 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L5 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L6 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L6 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L6 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L7 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L7 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L7 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L8 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L8 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L8 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L9 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L9 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L9 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L10 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L10 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L10 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L16 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L16 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L16 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L32 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L32 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L32 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

instance ToData L64 where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = \case {}

instance FromData L64 where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData _ = Nothing

instance UnsafeFromData L64 where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  -- "PT2" is `voidIsNotSupportedError`
  unsafeFromBuiltinData _ = traceError "PT2"

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
  Eq (Digest hf a)
  where
  (Digest x) == (Digest y) = x == y

instance
  (HashFunction hf) =>
  ToByteString (Digest hf a)
  where
  toByteString (Digest (FixedLengthByteString b)) = b
  byteStringOut (Digest b) = byteStringOut b

instance
  (HashFunction hf) =>
  FromByteString (Digest hf a)
  where
  --  fromByteString = fromByteStringIn
  byteStringIn isTerminal = byteStringIn isTerminal <&> Digest

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

-- instance (HashFunction hf) => FromByteString (DigestRef hf x) where
--  fromByteString = lookupDigest . fromByteString
--  byteStringIn isTerminal = byteStringIn isTerminal <&> lookupDigest
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

-- ** PubKey

instance Eq PubKey where
  (PubKey x) == (PubKey y) = x == y

instance Show PubKey where
  show (PubKey x) = "PubKey " <> show x

instance ToByteString PubKey where
  toByteString = toByteString . getPubKey
  byteStringOut (PubKey pk) = byteStringOut pk

instance FromByteString PubKey where
  --  fromByteString = PubKey . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> PubKey

-- ** PubKeyHash

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

digest :: (HashFunction hf, ToByteString a) => PlainText hf a -> Digest hf a
digest (PlainText m :: PlainText hf a) = computeDigest m

computeDigest :: (HashFunction hf, ToByteString a) => a -> Digest hf a
computeDigest a = hashFunction (toByteString a)

digestRef :: (HashFunction hf, ToByteString a) => a -> DigestRef hf a
digestRef x = DigestRef (computeDigest x) x

lookupDigest :: (HashFunction hf) => Digest hf a -> a
lookupDigest = traceError "Cannot get a value from its digest"

castDigest :: Digest hf a -> Digest hf b
castDigest (Digest x) = Digest x

computeHash :: (ToByteString a) => a -> DataHash
computeHash = computeDigest . toByteString

------------------------------------------------------------------------------
-- Multisig Verification
------------------------------------------------------------------------------

-- Function that checks if a SingleSig is valid
singleSigValid :: (ToByteString a) => a -> SingleSig -> Bool
singleSigValid message (SingleSig (pubKey, sig)) =
  verifyEd25519Signature (toByteString pubKey) (toByteString message) (toByteString sig)

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
              validSignatures = filter (\ss@(SingleSig (pubKey, sig)) -> pubKey `elem` pubKeys && singleSigValid message ss) singleSigs
           in length validSignatures >= toInt minSigs

-- * Meta declarations

makeLift ''FixedLengthByteString
makeIsDataIndexed ''FixedLengthByteString [('FixedLengthByteString, 0)]

makeLift ''Digest
makeIsDataIndexed ''Digest [('Digest, 0)]

makeTypeable (TH.ConT ''DefaultUni) ''Blake2b_256
makeTypeable (TH.ConT ''DefaultUni) ''L2
makeTypeable (TH.ConT ''DefaultUni) ''L3
makeTypeable (TH.ConT ''DefaultUni) ''L4
makeTypeable (TH.ConT ''DefaultUni) ''L5
makeTypeable (TH.ConT ''DefaultUni) ''L6
makeTypeable (TH.ConT ''DefaultUni) ''L7
makeTypeable (TH.ConT ''DefaultUni) ''L8
makeTypeable (TH.ConT ''DefaultUni) ''L9
makeTypeable (TH.ConT ''DefaultUni) ''L10
makeTypeable (TH.ConT ''DefaultUni) ''L16
makeTypeable (TH.ConT ''DefaultUni) ''L32
makeTypeable (TH.ConT ''DefaultUni) ''L64

makeLift ''UInt16
makeIsDataIndexed ''UInt16 [('UInt16, 0)]

makeLift ''PubKey
makeIsDataIndexed ''PubKey [('PubKey, 0)]

makeLift ''MultiSigPubKey
makeIsDataIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]

makeLift ''SingleSig
makeIsDataIndexed ''SingleSig [('SingleSig, 0)]

makeLift ''MultiSig
makeIsDataIndexed ''MultiSig [('MultiSig, 0)]

-- makeLift ''Blake2b_256

{-
PlutusTx.makeLift ''FixedLengthByteString
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
