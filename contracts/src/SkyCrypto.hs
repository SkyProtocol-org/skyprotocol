{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module SkyCrypto where

import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)

import PlutusTx.Prelude -- hiding (Applicative, Functor, fmap, pure, (<*>))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Show
import PlutusTx.Utils
import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
import PlutusLedgerApi.V1.Time (POSIXTime(..))
import PlutusLedgerApi.V1.Value (CurrencySymbol(..))

import SkyBase

-- * Types

-- List of pubkeys that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey { multiSigPubKeyKeys :: [PubKey], multiSigPubKeyThreshold :: UInt16 }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- A single signature by a single data operator public key
data SingleSig = SingleSig { singleSigPubKey :: PubKey, singleSigSignature :: Bytes64 }
  deriving (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Signatures produced by data operators for top hash
data MultiSig = MultiSig [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type DataDigest hf = Digest hf BuiltinByteString

type Hash = Digest Blake2b_256

type DataHash = Hash BuiltinByteString

type HashRef = DigestRef Blake2b_256

newtype PubKey = PubKey { getPubKey :: Bytes32 }
  deriving (Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

newtype Digest hf a = Digest { digestByteString :: FixedLengthByteString hf }
  deriving (Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Static intent to transform with a hash or encryption function f
newtype PlainText f a = PlainText a
  deriving (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

data Blake2b_256 -- static knowledge of hash function

data DigestRef hf x = DigestRef {digestRefValue :: x, digestRefDigest :: Digest hf x}
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- * Classes
class
  (StaticLength hf) =>
  HashFunction hf where
  hashFunction :: BuiltinByteString -> Digest hf a

class
  (HashFunction hf) =>
  DigestibleRef hf r
  where
  getDigest :: r a -> Digest hf a

-- * Instances

-- ** Digest
instance
  (HashFunction hf) =>
  Eq (Digest hf a) where -- The one from deriving isn't INLINEABLE by Plutus(!)
  (Digest x) == (Digest y) = x == y
instance
  (HashFunction hf) =>
  ByteStringIn (Digest hf a) where
  byteStringIn = byteStringIn <&> Digest
instance
  (HashFunction hf) =>
  FromByteString (Digest hf a) where
  fromByteString = fromByteStringIn
instance
  (HashFunction hf) =>
  ByteStringOut (Digest hf a) where
  byteStringOut (Digest b) = byteStringOut b
instance
  (HashFunction hf) =>
  ToByteString (Digest hf a) where
  toByteString (Digest (FixedLengthByteString b)) = b
instance
  (HashFunction hf) =>
  Dato (Digest hf a) where
instance
  (HashFunction hf) =>
  LiftByteStringIn (Digest hf) where
  liftByteStringIn = byteStringIn
instance
  (HashFunction hf) =>
  LiftEq (Digest hf) where
  liftEq = (==)
instance
  (HashFunction hf) =>
  LiftShow (Digest hf) where
  liftShow = show
instance
  (HashFunction hf) =>
  LiftDato (Digest hf) where
instance
  (HashFunction hf) =>
  LiftByteStringOut (Digest hf) where
  liftByteStringOut = byteStringOut
instance
  (HashFunction hf) =>
  LiftToByteString (Digest hf) where
  liftToByteString = toByteString

-- ** Blake2b_256
instance HashFunction Blake2b_256 where
  hashFunction = Digest . FixedLengthByteString . blake2b_256
instance StaticLength Blake2b_256 where
  staticLength = 32

-- ** DigestRef
instance HashFunction hf => DigestibleRef hf (DigestRef hf) where
  getDigest = digestRefDigest
instance
  (HashFunction hf) =>
  Eq (DigestRef hf x) where
  (DigestRef _ ah) == (DigestRef _ bh) = ah == bh
instance (HashFunction hf) => ByteStringOut (DigestRef hf x) where
  byteStringOut = byteStringOut . digestRefDigest
{-instance (HashFunction hf) => ByteStringIn (DigestRef hf x) where
  byteStringIn = byteStringIn <&> lookupDigest -}
instance (HashFunction hf, Show a) => Show (DigestRef hf a) where
  show (DigestRef x _) = "(digestRef $ " <> show x <> ")"
instance (HashFunction hf, Dato a) => PreWrapping Identity (DigestRef hf) a where
  wrap = return . digestRef
instance (HashFunction hf, Dato a) => Wrapping Identity (DigestRef hf) a where
  unwrap = return . digestRefValue
-- instance LiftShow (DigestRef hf) where
--   liftShow = show . digestRefDigest
instance
  (HashFunction hf) =>
  LiftByteStringOut (DigestRef hf) where
  liftByteStringOut = byteStringOut . digestRefDigest
instance
  (HashFunction hf) =>
  LiftToByteString (DigestRef hf) where
  liftToByteString = toByteString . digestRefDigest
instance
  (HashFunction hf) =>
  LiftShow (DigestRef hf) where
  liftShow = show
instance
  (HashFunction hf) =>
  LiftEq (DigestRef hf) where
  liftEq = (==)
instance
  (HashFunction hf) =>
  LiftDato (DigestRef hf) where
instance
  (HashFunction hf) =>
  LiftPreWrapping Identity (DigestRef hf) where
  liftWrap = wrap
instance
  (HashFunction hf) =>
  LiftWrapping Identity (DigestRef hf) where
  liftUnwrap = unwrap

-- ** LiftRef
instance
  (HashFunction hf, DigestibleRef hf r) =>
  DigestibleRef hf (LiftRef r) where
  getDigest = getDigest . liftref

-- ** PubKey
instance Eq PubKey where -- the one from deriving isn't INLINEABLE by Plutus!
  (PubKey x) == (PubKey y) = x == y
instance ToByteString PubKey where
  toByteString (PubKey (FixedLengthByteString pk)) = pk
instance FromByteString PubKey where
  fromByteString pk = PubKey (FixedLengthByteString pk)
instance ByteStringOut PubKey where
  byteStringOut (PubKey pk) = byteStringOut pk
instance ByteStringIn PubKey where
  byteStringIn = byteStringIn <&> PubKey

-- ** PubKeyHash
instance
  ByteStringIn PubKeyHash where
  byteStringIn = byteStringIn <&> PubKeyHash
instance
  ByteStringOut PubKeyHash where
  byteStringOut = byteStringOut . getPubKeyHash

-- * Helpers

digest :: (HashFunction hf, ToByteString a) => PlainText hf a -> Digest hf a
digest (PlainText m :: PlainText hf a) = computeDigest m

computeDigest :: (HashFunction hf, ToByteString a) => a -> Digest hf a
computeDigest a = hashFunction (toByteString a)

digestRef :: (HashFunction hf, ToByteString a) => a -> DigestRef hf a
digestRef x = DigestRef x (computeDigest x)

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
singleSigValid :: ToByteString a => a -> SingleSig -> Bool
singleSigValid message (SingleSig pubKey sig) =
  verifyEd25519Signature (toByteString pubKey) (toByteString message) (toByteString sig)

-- Main function to check if the MultiSig satisfies at least N valid unique signatures
multiSigValid :: ToByteString a => MultiSigPubKey -> a -> MultiSig -> Bool
multiSigValid (MultiSigPubKey pubKeys minSigs) message (MultiSig singleSigs) =
  let -- Extract the public keys from the SingleSig values
      pubKeysInSignatures = map (\(SingleSig pubKey _) -> pubKey) singleSigs
      -- Check for duplicates by comparing the list to its nub version
      noDuplicates = pubKeysInSignatures == nub pubKeysInSignatures
  in if not noDuplicates
     then False -- Duplicates found, return False
     else let -- Filter for valid signatures from required public keys
              validSignatures = filter (\ss@(SingleSig pubKey sig) -> pubKey `elem` pubKeys && singleSigValid message ss) singleSigs
          in length validSignatures >= toInt minSigs

-- * Meta declarations
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
