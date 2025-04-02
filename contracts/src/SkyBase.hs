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

-- (trace')
module SkyBase where
--module SkyBase (PreWrapping, Wrapping, LiftRef (..), LiftBinary, LiftShow, LiftEq, DigestibleRef, DigestRef (..), HashRef, HashAlgorithmOf, wrap, unwrap, integerLength, fbIntegerLength, fbuIntegerLength, lowestBitSet, lowestBitClear, fbLowestBitSet, fbLowestBitClear, fbuLowestBitClear, lowBitsMask, extractBitField, lookupDigest, liftEq, liftShow, liftGet, liftPut, getDigest, digestiblePut, computeDigest, trace', trace1, trace2, trace3, etrace1, etrace2, etrace3) where
-- DigestOnly (..),

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

import qualified GHC.Base as GB
import Control.Monad (Monad)
-- import Control.Monad.State.Lazy (State)
-- import Codec.Serialise (serialise, deserialise)
-- import Codec.Serialise.Class (Serialise, encode, decode, encodeList, decodeList)
-- import Codec.Serialise.Encoding (Encoding)
-- import Codec.Serialise.Decoding (Decoder)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
-- import Data.Maybe (fromJust) -- won't compile to Plutus (!) so we redefine it below.

-- * Types

data L4 -- staticLength is 4
data L8 -- staticLength is 8
data L32 -- staticLength is 32
data L64 -- staticLength is 64

-- | ByteString of statically known length
newtype
  (StaticLength len) =>
  FixedLengthByteString len = FixedLengthByteString BuiltinByteString
  deriving (Show)
  deriving anyclass (HasBlueprintDefinition)

-- NB: To fit on-chain on Cardano (or affordably on any L1,
-- and thus on any L2 that gets verified by a L2),
-- a VariableLengthByteString has to be of length <= 65535 (in practice smaller, more like 8192)
-- For larger data structures... put them in a Trie wherein only a logarithmic fragment is witnessed
newtype VariableLengthByteString = VariableLengthByteString BuiltinByteString
  deriving (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | Byte
newtype Byte = Byte { fromByte :: Integer }
  deriving (Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | UInt16
newtype UInt16 = UInt16 { fromUInt16 :: Integer }
  deriving (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | FixedLengthInteger
newtype
  (StaticLength len) =>
  FixedLengthInteger len = FixedLengthInteger Integer
  deriving (Show, Eq)
  deriving anyclass (HasBlueprintDefinition)

-- Make it a newtype VariableLengthInteger = VariableLengthInteger { getVli :: (Integer, Integer) } ???
-- | VariableLengthInteger
data VariableLengthInteger = VariableLengthInteger
  { vliBitLength :: Integer
  , vliInteger :: Integer }
  deriving Show
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | Pure Input from ByteString
newtype ByteStringReader a = ByteStringReader
  { getByteStringReader :: ByteStringCursor -> Maybe (a, ByteStringCursor) }

data ByteStringCursor = ByteStringCursor
  { cursorByteString :: BuiltinByteString -- the bytes
  , cursorStart :: Integer -- start index included
  , cursorEnd :: Integer } -- end index not included
  deriving (Show, Eq)
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

-- | Wrapper for (r a)
newtype LiftRef r a = LiftRef {liftref :: r a}
  deriving (Functor)

-- | Fixed-Point
data Fix f = Fix { getFix :: f (Fix f) }
-- not newtype because Plutus doesn't support recursive newtype

-- ** Fixed size data structures
type Bytes4 = FixedLengthByteString L4
type Bytes8 = FixedLengthByteString L8
type Bytes32 = FixedLengthByteString L32
type Bytes64 = FixedLengthByteString L64

type UInt32 = FixedLengthInteger L4
type UInt64 = FixedLengthInteger L8
type UInt256 = FixedLengthInteger L32

-- ** Cardano specific types
type DataDigest hf = Digest hf BuiltinByteString

type Hash = Digest Blake2b_256

type DataHash = Hash BuiltinByteString

type HashRef = DigestRef Blake2b_256

newtype PubKey = PubKey { getPubKey :: Bytes32 }
  deriving (Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- * Classes

-- | Partial types
class Partial a where
  isElement :: a -> Bool
  validate :: a -> a
  validate a = if isElement a then a else traceError "Bad value"

-- | Static Length data
class StaticLength i where
  staticLength :: Integer

class ToInt a where
  toInt :: a -> Integer

class FromInt a where
  fromInt :: Integer -> a

class ToByteString a where
  toByteString :: a -> BuiltinByteString

class FromByteString a where
  fromByteString :: BuiltinByteString -> a

class ByteStringOut a where
  byteStringOut :: a -> BuiltinByteString -> BuiltinByteString

class ByteStringIn a where
  byteStringIn :: ByteStringReader a

{- Failure to include bitLength in either Haskell or Plutus seems incompetent to me. --fare
   (see CIP-123, compare to CLHS integer-length) -}
class HasBitLogic a where
  bitLength :: a -> Integer
  lowestBitClear :: a -> Integer -- NB: returns -1 for 0, not maxBitLength, even for non-negative types!
  isBitSet :: Integer -> a -> Bool
  lowBitsMask :: Integer -> a
  logicalOr :: a -> a -> a
  logicalAnd :: a -> a -> a
  logicalXor :: a -> a -> a
  shiftRight :: a -> Integer -> a
  shiftLeft :: a -> Integer -> a
  shiftLeftWithBits :: a -> Integer -> a -> a
  shiftLeftWithBits a l b = (a `shiftLeft` l) `logicalOr` b

class
  (StaticLength hf) =>
  HashFunction hf where
  hashFunction :: BuiltinByteString -> Digest hf a

class
  (HashFunction hf) =>
  DigestibleRef hf r
  where
  getDigest :: r a -> Digest hf a

class
  (ByteStringOut d, Show d, Eq d) =>
  Dato d

class
  (LiftByteStringOut r, LiftShow r, LiftEq r) =>
  LiftDato r

class LiftEq r where
  liftEq :: (Eq a) => r a -> r a -> Bool

class LiftShow r where
  liftShow :: (Show a) => r a -> BuiltinString

class LiftByteStringOut r where
  liftByteStringOut :: (ByteStringOut a) => r a -> BuiltinByteString -> BuiltinByteString

class LiftByteStringIn r where
  liftByteStringIn :: (ByteStringIn a) => ByteStringReader (r a)

class
  (Monad e) =>
  PreWrapping a r e
  where
  wrap :: a -> e (r a)

-- | Wrapping : a value can be wrapped, and wrapped value that can be unwrapped
class
  (PreWrapping a r e) =>
  Wrapping a r e
  where
  unwrap :: r a -> e a

-- * Instances

-- ** StaticLength
instance StaticLength L4 where
  staticLength = 4
instance StaticLength L8 where
  staticLength = 8
instance StaticLength L32 where
  staticLength = 32
instance StaticLength L64 where
  staticLength = 64

-- ** FixedLengthByteString
instance
  (StaticLength len) =>
  Eq (FixedLengthByteString len) where -- the one from deriving isn't INLINEABLE by Plutus(!)
  (FixedLengthByteString a) == (FixedLengthByteString b) = a == b
instance
  (StaticLength len) =>
  Partial (FixedLengthByteString len) where
  isElement (FixedLengthByteString b) = lengthOfByteString b == staticLength @len
instance
  (StaticLength len) =>
  ToInt (FixedLengthByteString len) where
  toInt (FixedLengthByteString b) = toInt b
instance
  (StaticLength len) =>
  FromInt (FixedLengthByteString len) where
  fromInt = FixedLengthByteString . integerToByteString BigEndian (staticLength @len)
instance
  (StaticLength len) =>
  ToByteString (FixedLengthByteString len) where
  toByteString (FixedLengthByteString b) = b
instance
  (StaticLength len) =>
  FromByteString (FixedLengthByteString len) where
  fromByteString = validate . FixedLengthByteString
instance
  (StaticLength len) =>
  HasBitLogic (FixedLengthByteString len) where
  bitLength (FixedLengthByteString b) = bitLength b
  lowestBitClear (FixedLengthByteString b) = lowestBitClear b
  isBitSet n (FixedLengthByteString b) = readBit b n
  lowBitsMask l = FixedLengthByteString $ shiftByteString (replicateByte 0xFF $ staticLength @len) $ -l
  logicalOr (FixedLengthByteString a) (FixedLengthByteString b) =
    FixedLengthByteString $ orByteString False a b
  logicalAnd (FixedLengthByteString a) (FixedLengthByteString b) =
    FixedLengthByteString $ andByteString False a b
  logicalXor (FixedLengthByteString a) (FixedLengthByteString b) =
    FixedLengthByteString $ xorByteString False a b
  shiftRight (FixedLengthByteString b) i = FixedLengthByteString $ shiftByteString b $ -i
  shiftLeft (FixedLengthByteString b) i = FixedLengthByteString $ shiftByteString (toByteString b) i
instance ByteStringOut (FixedLengthByteString len) where
  byteStringOut (FixedLengthByteString b) = appendByteString b
instance
  (StaticLength len) =>
  ByteStringIn (FixedLengthByteString len) where
  byteStringIn =
    byteStringInByteString (staticLength @len) <&> FixedLengthByteString
instance
  (StaticLength len) =>
  Dato (FixedLengthByteString len) where

-- ** VariableLengthByteString
instance Partial VariableLengthByteString where
  isElement (VariableLengthByteString b) = lengthOfByteString b <= 65535
instance
  ToInt VariableLengthByteString where
  toInt (VariableLengthByteString b) = toInt b
instance
  FromInt VariableLengthByteString where
  fromInt = VariableLengthByteString . toByteString
instance
  ToByteString VariableLengthByteString where
  toByteString (VariableLengthByteString b) = b
instance
  FromByteString VariableLengthByteString where
  fromByteString = VariableLengthByteString
instance
  FromByteString VariableLengthInteger where
  fromByteString b = VariableLengthInteger (bitLength b) (byteStringToInteger BigEndian b)
instance HasBitLogic VariableLengthByteString where
  bitLength (VariableLengthByteString b) = bitLength b
  lowestBitClear (VariableLengthByteString b) = lowestBitClear b
  isBitSet n (VariableLengthByteString b) = isBitSet n b
  lowBitsMask l = VariableLengthByteString $ lowBitsMask l
  logicalOr (VariableLengthByteString a) (VariableLengthByteString b) =
    VariableLengthByteString $ logicalOr a b
  logicalAnd (VariableLengthByteString a) (VariableLengthByteString b) =
    VariableLengthByteString $ logicalAnd a b
  logicalXor (VariableLengthByteString a) (VariableLengthByteString b) =
    VariableLengthByteString $ logicalXor a b
  shiftRight (VariableLengthByteString b) i = VariableLengthByteString $ shiftRight b i
  shiftLeft (VariableLengthByteString b) i = VariableLengthByteString $ shiftLeft b i
instance ByteStringOut VariableLengthByteString where
  byteStringOut (VariableLengthByteString b) = byteStringOut b
instance ByteStringIn VariableLengthByteString where
  byteStringIn =
    byteStringIn >>= \ (UInt16 len) ->
    byteStringInByteString len <&> VariableLengthByteString
instance Dato VariableLengthByteString where

-- ** BuiltinByteString
instance
  ToInt BuiltinByteString where
  toInt b = byteStringToInteger BigEndian b
instance ToByteString BuiltinByteString where
  toByteString x = x
instance
  ByteStringIn BuiltinByteString where
  byteStringIn = byteStringIn <&> toByteString @VariableLengthByteString
instance HasBitLogic BuiltinByteString where
  bitLength b =
    let len = lengthOfByteString b
        loop i = if i >= len then 0 else
          let byte = indexByteString b i in
            if byte > 0 then bitLength8 byte + len - i else loop $ i + 1 in
        loop 0
  lowestBitClear b = findFirstSetBit $ complementByteString b
  isBitSet = flip readBit
  lowBitsMask l = shiftByteString (replicateByte 0xFF $ bitLengthToByteLength l) $ (-l) `quotient` 8
  logicalOr a b = let (a', b') = equalizeByteStringLength a b in orByteString False a' b'
  logicalAnd a b = let (a', b') = equalizeByteStringLength a b in andByteString False a' b'
  logicalXor a b = let (a', b') = equalizeByteStringLength a b in xorByteString False a' b'
  shiftRight b i = shiftByteString b $ -i
  shiftLeft b i = shiftByteString (toByteString b) i
instance ByteStringOut BuiltinByteString where
  byteStringOut b s =
    let len = toUInt16 $ lengthOfByteString b in
    appendByteString (toByteString len) $ appendByteString b s
instance Dato BuiltinByteString where

-- ** Byte
instance Eq Byte where -- deriving (PlutusTx.Eq) leads to un-INLINEABLE ==
  (Byte a) == (Byte b) = a == b
instance Partial Byte where
  isElement (Byte b) = 0 <= b && b <= 255
instance ToInt Byte where
  toInt = fromByte
instance FromInt Byte where
  fromInt = toByte
instance ToByteString Byte where
  toByteString (Byte n) = integerToByteString BigEndian 1 n
instance FromByteString Byte where
  fromByteString = toByte . byteStringToInteger BigEndian
instance HasBitLogic Byte where
  bitLength (Byte n) = bitLength8 n
  lowestBitClear (Byte b) = findFirstSetBit $ toByteString $ 255-b
  isBitSet n b = readBit (toByteString b) n
  lowBitsMask l = Byte $ indexByteString (shiftByteString byteString1 $ l - 8) 0
  logicalOr a b = Byte $ indexByteString (orByteString False (toByteString a) (toByteString b)) 0
  logicalAnd a b = Byte $ indexByteString (andByteString False (toByteString a) (toByteString b)) 0
  logicalXor a b = Byte $ indexByteString (xorByteString False (toByteString a) (toByteString b)) 0
  shiftRight b i = Byte $ indexByteString (shiftByteString (toByteString b) $ -i) 0
  shiftLeft b i = Byte $ indexByteString (shiftByteString (toByteString b) i) 0
  shiftLeftWithBits (Byte a) l (Byte b) = Byte $ (a `shiftLeft` l) + b
instance ByteStringOut Byte where
  byteStringOut (Byte b) = consByteString b
instance ByteStringIn Byte where
  byteStringIn = ByteStringReader nextByteStringCursor
instance Dato Byte where

-- ** UInt16
instance Partial UInt16 where
  isElement (UInt16 b) = 0 <= b && b <= 65535
instance ToInt UInt16 where
  toInt = fromUInt16
instance FromInt UInt16 where
  fromInt = toUInt16
instance ToByteString UInt16 where
  toByteString (UInt16 n) = integerToByteString BigEndian 2 n
instance FromByteString UInt16 where
  fromByteString = toUInt16 . byteStringToInteger BigEndian
instance HasBitLogic UInt16 where
  bitLength (UInt16 n) = bitLength16 n
  lowestBitClear (UInt16 b) = findFirstSetBit $ toByteString $ 65535-b
  isBitSet n b = readBit (toByteString b) n
  lowBitsMask l = UInt16 $ indexByteString (shiftByteString byteString2 $ l - 16) 0
  logicalOr a b = UInt16 $ indexByteString (orByteString False (toByteString a) (toByteString b)) 0
  logicalAnd a b = UInt16 $ indexByteString (andByteString False (toByteString a) (toByteString b)) 0
  logicalXor a b = UInt16 $ indexByteString (xorByteString False (toByteString a) (toByteString b)) 0
  shiftRight b i = UInt16 $ indexByteString (shiftByteString (toByteString b) $ -i) 0
  shiftLeft b i = UInt16 $ indexByteString (shiftByteString (toByteString b) i) 0
  shiftLeftWithBits (UInt16 a) l (UInt16 b) = UInt16 $ (a `shiftLeft` l) + b
instance ByteStringOut UInt16 where
  byteStringOut = appendByteString . toByteString
instance ByteStringIn UInt16 where
  byteStringIn = byteStringIn >>= \ (Byte hi) ->
             byteStringIn <&> \ (Byte lo) ->
             UInt16 (hi * 256 + lo)
instance Dato UInt16 where

-- ** FixedLengthInteger
instance
  (StaticLength len) =>
  Partial (FixedLengthInteger len) where
  isElement (FixedLengthInteger i) = i <= maxValue
    where maxValue = (exponential 2 (staticLength @len)) - 1
  validate f@(FixedLengthInteger i) =
    let b = integerToByteString BigEndian (staticLength @len) i in
      if b == b then f else traceError "Bad integer"
instance
  ToInt (FixedLengthInteger len) where
  toInt (FixedLengthInteger n) = n
instance
  (StaticLength len) =>
  FromInt (FixedLengthInteger len) where
  fromInt = validate . FixedLengthInteger
instance
  (StaticLength len) =>
  ToByteString (FixedLengthInteger len) where
  toByteString (FixedLengthInteger n) = integerToByteString BigEndian (staticLength @len) n
instance
  (StaticLength len) =>
  HasBitLogic (FixedLengthInteger len) where
  bitLength (FixedLengthInteger n) = bitLength $ integerToByteString LittleEndian (staticLength @len) n
  lowestBitClear n = lowestBitClear $ toByteString n
  isBitSet n i = isBitSet n (toByteString i)
  lowBitsMask l = FixedLengthInteger . toInt $ (lowBitsMask l :: FixedLengthByteString len)
  logicalOr (FixedLengthInteger a) (FixedLengthInteger b) =
    FixedLengthInteger . toInt $ logicalOr (fromInt a :: FixedLengthByteString len) (fromInt b)
  logicalAnd (FixedLengthInteger a) (FixedLengthInteger b) =
    FixedLengthInteger . toInt $ logicalAnd (fromInt a :: FixedLengthByteString len) (fromInt b)
  logicalXor (FixedLengthInteger a) (FixedLengthInteger b) =
    FixedLengthInteger . toInt $ logicalXor (fromInt a :: FixedLengthByteString len) (fromInt b)
  shiftRight (FixedLengthInteger b) i = FixedLengthInteger $ shiftRight b i
  shiftLeft (FixedLengthInteger b) i = FixedLengthInteger . toInt $ shiftByteString (toByteString b) i
  shiftLeftWithBits (FixedLengthInteger a) l (FixedLengthInteger b) = FixedLengthInteger $ (a `shiftLeft` l) + b
instance
  (StaticLength len) =>
  ByteStringOut (FixedLengthInteger len) where
  byteStringOut (FixedLengthInteger i) =
    appendByteString (integerToByteString BigEndian (staticLength @len) i)
instance
  (StaticLength len) =>
  ByteStringIn (FixedLengthInteger len) where
  byteStringIn = byteStringInByteString (staticLength @len) <&>
    FixedLengthInteger . byteStringToInteger BigEndian
instance
  (StaticLength len) =>
  FromByteString (FixedLengthInteger len) where
  fromByteString bs =
    if lengthOfByteString bs > staticLength @len then traceError "ByteString too long" else
      FixedLengthInteger $ byteStringToInteger BigEndian bs
instance
  (StaticLength len) =>
  Dato (FixedLengthInteger len) where

-- ** VariableLengthInteger
{- Limitation:
 - only integers of length less than 65536
 - I/O only for non-negative integers (for now)
 - we do not reject non-canonical input (leading zeros) -}
instance Eq VariableLengthInteger where
  x == y = vliInteger x == vliInteger y
instance
  ToInt VariableLengthInteger where
  toInt = vliInteger
instance
  FromInt VariableLengthInteger where
  fromInt n = VariableLengthInteger (bitLength n) n
instance
  ToByteString VariableLengthInteger where
  toByteString (VariableLengthInteger l n) = integerToByteString BigEndian (bitLengthToByteLength l) n
instance
  FromByteString BuiltinByteString where
  fromByteString x = x
instance
  ByteStringIn Integer where
  byteStringIn = byteStringIn <&> toInt @VariableLengthByteString
instance HasBitLogic VariableLengthInteger where
  bitLength (VariableLengthInteger l _) = l
  lowestBitClear n = lowestBitClear $ toByteString n
  isBitSet n i = isBitSet n (toByteString i)
  lowBitsMask l = VariableLengthInteger l $ lowBitsMask l
  logicalOr a b =
    let v = logicalOr (VariableLengthByteString $ toByteString a)
                      (VariableLengthByteString $ toByteString b) in
    VariableLengthInteger (bitLength v) $ toInt v
  logicalAnd a b =
    let v = logicalAnd (VariableLengthByteString $ toByteString a)
                       (VariableLengthByteString $ toByteString b) in
    VariableLengthInteger (bitLength v) $ toInt v
  logicalXor a b =
    let v = logicalXor (VariableLengthByteString $ toByteString a)
                       (VariableLengthByteString $ toByteString b) in
    VariableLengthInteger (bitLength v) $ toInt v
  shiftRight (VariableLengthInteger l b) i = VariableLengthInteger (l - i `max` 0) $ shiftRight b i
  shiftLeft (VariableLengthInteger l b) i = VariableLengthInteger (l + i) $ shiftLeft b i
  shiftLeftWithBits (VariableLengthInteger la a) l vb@(VariableLengthInteger _ b) =
    if la == 0 then vb else VariableLengthInteger (la + l) $ (a `shiftLeft` l) + b
instance ByteStringOut VariableLengthInteger where
  byteStringOut = byteStringOut . toByteString
instance Dato VariableLengthInteger where

-- ** Integer
instance ToByteString Integer where
  toByteString n = integerToByteString BigEndian (bitLengthToByteLength $ bitLength n) n
instance HasBitLogic Integer where
  bitLength n =
    if n < 0 then
      bitLength (-n - 1) -- two's complement notion of bit length
    else if n <= 65535 then
        bitLength16 n
    else let findLen l m = if n < m then l else findLen (l + l) (m * m)
             len = findLen 4 4294967296 in
           bitLength $ integerToByteString LittleEndian len n
  lowestBitClear n = lowestBitSet (-n-1) where
    lowestBitSet n = if n == 0 then -1 else up n 1 2 []
    up n i m ms = let (q, r) = n `divMod` m in
                    if r == 0 then up q (i + i) (m * m) (m : ms)
                    else down ms i r (i - 1)
    down [] _ _ h = h
    down (m : ms) i n h =
      let j = i `divide` 2
          (q, r) = n `divMod` m in
        if r == 0 then down ms j (h + j) q
        else down ms j h r
  isBitSet i n = let e = exponential 2 i in n `divide` (e + e) >= e
  lowBitsMask l = (exponential 2 l) - 1
  logicalOr a b = toInt $ logicalOr (VariableLengthByteString $ toByteString a)
                                    (VariableLengthByteString $ toByteString b)
  logicalAnd a b = toInt $ logicalAnd (VariableLengthByteString $ toByteString a)
                                      (VariableLengthByteString $ toByteString b)
  logicalXor a b = toInt $ logicalXor (VariableLengthByteString $ toByteString a)
                                      (VariableLengthByteString $ toByteString b)
  shiftRight b i = b `divide` exponential 2 i
  shiftLeft b i = b * exponential 2 i
  shiftLeftWithBits a l b = (a `shiftLeft` l) + b
instance ByteStringOut Integer where
  byteStringOut = byteStringOut . toByteString
instance Dato Integer where

-- ** ByteStringReader
instance GB.Functor ByteStringReader where
  fmap f (ByteStringReader r) = ByteStringReader $ \ s ->
    r s <&> \ (a, s') -> (f a, s')
instance Functor ByteStringReader where
  fmap = GB.fmap
instance Applicative ByteStringReader where
  pure = GB.pure
  (<*>) = (GB.<*>)
instance GB.Applicative ByteStringReader where
  pure a = ByteStringReader $ \ s -> Just (a, s)
  ByteStringReader x <*> ByteStringReader y = ByteStringReader $ \s ->
    x s >>= \ (f, s') ->
    y s' <&> \ (v, s'') -> (f v, s'')
instance Monad ByteStringReader where
  m >>= f =
    ByteStringReader (\s -> getByteStringReader m s >>=
      \ (a, s') -> getByteStringReader (f a) s')

-- ** POSIXTime
instance
  Show POSIXTime where
  show (POSIXTime x) = "(POSIXTime " <> show x <> ")"
instance
  ByteStringIn POSIXTime where
  byteStringIn = byteStringIn <&> POSIXTime
instance
  ByteStringOut POSIXTime where
  byteStringOut = byteStringOut . getPOSIXTime

-- ** CurrencySymbol
instance
  ByteStringIn CurrencySymbol where
  byteStringIn = byteStringIn <&> CurrencySymbol
instance
  ByteStringOut CurrencySymbol where
  byteStringOut = byteStringOut . unCurrencySymbol

-- ** PubKeyHash
instance
  ByteStringIn PubKeyHash where
  byteStringIn = byteStringIn <&> PubKeyHash
instance
  ByteStringOut PubKeyHash where
  byteStringOut = byteStringOut . getPubKeyHash

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
-- instance LiftShow (DigestRef hf) where
--   liftShow = show . digestRefDigest
instance
  (HashFunction hf) =>
  LiftByteStringOut (DigestRef hf) where
  liftByteStringOut = byteStringOut . digestRefDigest
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
instance (ToByteString a, HashFunction hf) => PreWrapping a (LiftRef (DigestRef hf)) Identity where
  wrap x = Identity (LiftRef $ DigestRef x $ (computeDigest @a @hf x))
instance (ToByteString a, HashFunction hf) => Wrapping a (LiftRef (DigestRef hf)) Identity where
  unwrap (LiftRef (DigestRef x _)) = Identity x

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

-- PlutusTx only has this for pairs, not longer tuples
-- ** Tuples
instance
  (Eq a, Eq b, Eq c) =>
  Eq (a, b, c) where
  (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'
instance
  (Eq a, Eq b, Eq c, Eq d) =>
  Eq (a, b, c, d) where
  (a, b, c, d) == (a', b', c', d') = a == a' && b == b' && c == c' && d == d'
instance
  (Eq a, Eq b, Eq c, Eq d, Eq e) =>
  Eq (a, b, c, d, e) where
  (a, b, c, d, e) == (a', b', c', d', e') = a == a' && b == b' && c == c' && d == d' && e == e'
instance
  (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
  Eq (a, b, c, d, e, f) where
  (a, b, c, d, e, f) == (a', b', c', d', e', f') = a == a' && b == b' && c == c' && d == d' && e == e' && f == f'
instance ByteStringOut () where
  byteStringOut () s  = s
instance
  (ByteStringOut a, ByteStringOut b) =>
  ByteStringOut (a, b) where
  byteStringOut (a, b) s =
    byteStringOut a $
    byteStringOut b s
instance
  (ByteStringOut a, ByteStringOut b) =>
  ToByteString (a, b) where
  toByteString = toByteStringOut
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c) =>
  ByteStringOut (a, b, c) where
  byteStringOut (a, b, c) s =
    byteStringOut a $
    byteStringOut b $
    byteStringOut c s
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c) =>
  ToByteString (a, b, c) where
  toByteString = toByteStringOut
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d) =>
  ByteStringOut (a, b, c, d) where
  byteStringOut (a, b, c, d) s =
    byteStringOut a $
    byteStringOut b $
    byteStringOut c $
    byteStringOut d s
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d) =>
  ToByteString (a, b, c, d) where
  toByteString = toByteStringOut
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d, ByteStringOut e) =>
  ByteStringOut (a, b, c, d, e) where
  byteStringOut (a, b, c, d, e) s =
    byteStringOut a $
    byteStringOut b $
    byteStringOut c $
    byteStringOut d $
    byteStringOut e s
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d, ByteStringOut e) =>
  ToByteString (a, b, c, d, e) where
  toByteString = toByteStringOut
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d, ByteStringOut e, ByteStringOut f) =>
  ByteStringOut (a, b, c, d, e, f) where
  byteStringOut (a, b, c, d, e, f) s =
    byteStringOut a $
    byteStringOut b $
    byteStringOut c $
    byteStringOut d $
    byteStringOut e $
    byteStringOut f s
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d, ByteStringOut e, ByteStringOut f) =>
  ToByteString (a, b, c, d, e, f) where
  toByteString = toByteStringOut
instance ByteStringIn () where
  byteStringIn = return ()
instance
  (ByteStringIn a, ByteStringIn b) =>
  ByteStringIn (Either a b) where
  byteStringIn = byteStringIn >>= \ b ->
    if b == Byte 0 then byteStringIn <&> Left
    else if b == Byte 1 then byteStringIn <&> Right
    else byteStringReaderFail
instance
  (ByteStringIn a, ByteStringIn b) =>
  ByteStringIn (a, b) where
  byteStringIn = byteStringIn >>= \ a ->
                 byteStringIn >>= \ b ->
                 return (a, b)
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c) =>
  ByteStringIn (a, b, c) where
  byteStringIn = byteStringIn >>= \ a ->
                 byteStringIn >>= \ b ->
                 byteStringIn >>= \ c ->
                 return (a, b, c)
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c, ByteStringIn d) =>
  ByteStringIn (a, b, c, d) where
  byteStringIn = byteStringIn >>= \ a ->
                 byteStringIn >>= \ b ->
                 byteStringIn >>= \ c ->
                 byteStringIn >>= \ d ->
                 return (a, b, c, d)
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c, ByteStringIn d, ByteStringIn e) =>
  ByteStringIn (a, b, c, d, e) where
  byteStringIn = byteStringIn >>= \ a ->
                 byteStringIn >>= \ b ->
                 byteStringIn >>= \ c ->
                 byteStringIn >>= \ d ->
                 byteStringIn >>= \ e ->
                 return (a, b, c, d, e)
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c, ByteStringIn d, ByteStringIn e, ByteStringIn f) =>
  ByteStringIn (a, b, c, d, e, f) where
  byteStringIn = byteStringIn >>= \ a ->
                 byteStringIn >>= \ b ->
                 byteStringIn >>= \ c ->
                 byteStringIn >>= \ d ->
                 byteStringIn >>= \ e ->
                 byteStringIn >>= \ f ->
                 return (a, b, c, d, e, f)

-- ** Lists
instance
  (ByteStringOut a) =>
  ByteStringOut [a] where -- length limit 65535
  byteStringOut l s =
    byteStringOut (toUInt16 $ length l) $ foldr byteStringOut s l
instance (ByteStringOut a) => ToByteString [a] where
  toByteString = toByteStringOut
instance
  (ByteStringIn a) =>
  ByteStringIn [a] where -- length limit 65535
  byteStringIn = byteStringIn >>= \ (UInt16 len) -> loop len where
    loop n = if n == 0 then return [] else
      byteStringIn >>= \ a -> loop (n - 1) <&> (a :)

-- ** Identity
instance Eq a => Eq (Identity a) where
  x == y = (runIdentity x) == (runIdentity y)
instance LiftEq Identity where
  liftEq = (==)
instance LiftShow Identity where
  liftShow = show . runIdentity
{-
instance LiftSerialise Identity where
  liftEncode = encode . runIdentity
  liftDecode = decode <&> Identity
  liftEncodeList = encodeList . map runIdentity
  liftDecodeList = decodeList <&> map Identity
-}
instance PreWrapping a Identity Identity where
  wrap = Identity . Identity
instance Wrapping a Identity Identity where
  unwrap x = x
instance PreWrapping a (LiftRef Identity) Identity where
  wrap = Identity . LiftRef . Identity
instance Wrapping a (LiftRef Identity) Identity where
  unwrap = liftref

-- ** Fix: Y-combinator or fixed point combinator for types
{-
instance
  (LiftSerialise f) =>
  Serialise (Fix f)
  where
  encode = liftEncode . out
  decode = liftDecode <&> In
  encodeList = liftEncodeList . map out
  decodeList = liftDecodeList <&> map In
-}
instance
  (LiftByteStringOut f) =>
  ByteStringOut (Fix f) where
  byteStringOut (Fix x) = liftByteStringOut x
instance
  (LiftByteStringOut f) =>
  ToByteString (Fix f) where
  toByteString = toByteStringOut
instance
  (LiftEq f) =>
  Eq (Fix f) where
  (==) x y = liftEq (getFix x) (getFix y)
instance
  (LiftShow f) =>
  Show (Fix f) where
  show = liftShow . getFix
instance
  (LiftDato r) =>
  Dato (Fix r) where
instance
  (LiftByteStringIn f) =>
  ByteStringIn (Fix f) where
  byteStringIn = liftByteStringIn <&> Fix
instance
  (LiftByteStringIn f) =>
  FromByteString (Fix f) where
  fromByteString = fromByteStringIn

-- ** Dato
instance
  (Dato a, Dato b) =>
  Dato (a, b) where
instance
  (Dato a, Dato b, Dato c) =>
  Dato (a, b, c) where
instance
  (Dato a, Dato b, Dato c, Dato d) =>
  Dato (a, b, c, d) where
instance
  (Dato a, Dato b, Dato c, Dato d, Dato e) =>
  Dato (a, b, c, d, e) where
instance
  (Dato a, Dato b, Dato c, Dato d, Dato e, Dato f) =>
  Dato (a, b, c, d, e, f) where

-- ** LiftRef
instance
  (LiftEq r, Eq a) =>
  Eq (LiftRef r a)
  where
  (==) x y = liftEq (liftref x) (liftref y)
instance
  (LiftShow r, Show a) =>
  Show (LiftRef r a)
  where
  show = liftShow . liftref
instance
  (LiftDato r, Dato a) =>
  Dato (LiftRef r a)
  where
instance
  (LiftByteStringOut r, ByteStringOut a) =>
  ByteStringOut (LiftRef r a)
  where
  byteStringOut = liftByteStringOut . liftref
instance
  (LiftByteStringIn r, ByteStringIn a) =>
  ByteStringIn (LiftRef r a)
  where
  byteStringIn = liftByteStringIn <&> LiftRef

-- * Helpers

-- | Data.Maybe.fromJust reimplemented in Plutus-friendly way
fromJust :: Maybe a -> a
fromJust (Just a) = a

-- | How is this not in Plutus already?
multiplyByExponential :: Integer -> Integer -> Integer -> Integer
multiplyByExponential a e n =
  if n == 0 then a else
    let a' = if n `modulo` 2 == 1 then a * e else a
        e' = e * e
        n' = n `divide` 2 in
    multiplyByExponential a' e' n'

-- | How is this not in Plutus already?
exponential :: Integer -> Integer -> Integer
exponential = multiplyByExponential 1

toByte :: Integer -> Byte
toByte n = validate (Byte n)
toUInt16 :: Integer -> UInt16
toUInt16 n = validate (UInt16 n)

equalizeByteStringLength :: BuiltinByteString -> BuiltinByteString -> (BuiltinByteString, BuiltinByteString)
equalizeByteStringLength a b =
  let la = lengthOfByteString a
      lb = lengthOfByteString b in
    if la < lb then (appendByteString (replicateByte 0 $ lb - la) a,b)
    else if la > lb then (a, appendByteString (replicateByte 0 $ la - lb) b)
    else (a, b)
bitLengthToByteLength :: Integer -> Integer
bitLengthToByteLength n = (n + 7) `divide` 8
-- First argument is the length, second is the start (little endian), last is the bits
extractBitField :: HasBitLogic a => Integer -> Integer -> a -> a
extractBitField length height bits = (bits `shiftRight` height) `logicalAnd` lowBitsMask length

bitLength16 :: Integer -> Integer -- assumes input in [0,65535]
bitLength16 n = if n < 256 then bitLength8 n else 8 + bitLength8 (n `divide` 256)
bitLength8 :: Integer -> Integer -- assumes input in [0,255]
bitLength8 n = if n < 16 then bitLength4 n else 4 + bitLength4 (n `divide` 16)
bitLength4 :: Integer -> Integer -- assumes input in [0,15]
bitLength4 n = if n < 4 then bitLength2 n else 2 + bitLength2 (n `divide` 4)
bitLength2 :: Integer -> Integer -- assumes input in [0,3]
bitLength2 n = if n < 3 then n else 2
byteString1 = integerToByteString BigEndian 1 0xFF
byteString2 = integerToByteString BigEndian 2 0xFFFF

{-type ByteStringWriter a = State (BuiltinByteString -> BuiltinByteString) a
writeByteString :: ByteStringOut a => a -> ByteStringWriter ()
writeByteString a = get >>= \ suffix -> put (byteStringOut a . suffix)
byteStringWriterResult :: ByteStringWriter a -> BuiltinByteString
byteStringWriterResult m = execState m emptyByteString-}
toByteStringOut :: ByteStringOut a => a -> BuiltinByteString
toByteStringOut a = byteStringOut a emptyByteString

byteStringInByteString :: Integer -> ByteStringReader BuiltinByteString
byteStringInByteString n =
  ByteStringReader $ \ bsc ->
    let next = cursorStart bsc + n in
      if next <= cursorEnd bsc then
        Just (sliceByteString (cursorStart bsc) n (cursorByteString bsc),
              ByteStringCursor (cursorByteString bsc) next (cursorEnd bsc))
      else Nothing
byteStringReaderFail :: ByteStringReader a
byteStringReaderFail = ByteStringReader $ \ s -> Nothing
maybeFromByteStringIn :: ByteStringIn a => BuiltinByteString -> Maybe a
maybeFromByteStringIn bs = byteStringCursor bs &
  getByteStringReader byteStringIn >>= \ (a, bs') ->
  if emptyByteStringCursor bs' then Just a else Nothing
fromByteStringIn :: ByteStringIn a => BuiltinByteString -> a
fromByteStringIn = fromJust . maybeFromByteStringIn

byteStringCursor :: BuiltinByteString -> ByteStringCursor
byteStringCursor bs = ByteStringCursor bs 0 (lengthOfByteString bs)
emptyByteStringCursor :: ByteStringCursor -> Bool
emptyByteStringCursor bsc = cursorStart bsc >= cursorEnd bsc
nextByteStringCursor :: ByteStringCursor -> Maybe (Byte, ByteStringCursor)
nextByteStringCursor bsc =
  if emptyByteStringCursor bsc then Nothing else
    Just (Byte $ indexByteString (cursorByteString bsc) (cursorStart bsc),
          ByteStringCursor (cursorByteString bsc) (cursorStart bsc + 1) (cursorEnd bsc))

uncurry3 :: (a->b->c->d)->(a,b,c)->d
uncurry3 f (a,b,c) = f a b c
uncurry4 :: (a->b->c->d->e)->(a,b,c,d)->e
uncurry4 f (a,b,c,d) = f a b c d
uncurry5 :: (a->b->c->d->e->f)->(a,b,c,d,e)->f
uncurry5 f (a,b,c,d,e) = f a b c d e
uncurry6 :: (a->b->c->d->e->f->g)->(a,b,c,d,e,f)->g
uncurry6 g (a,b,c,d,e,f) = g a b c d e f
curry3 :: ((a,b,c)->d)->a->b->c->d
curry3 f a b c = f (a, b, c)
curry4 :: ((a,b,c,d)->e)->a->b->c->d->e
curry4 f a b c d = f (a, b, c, d)
curry5 :: ((a,b,c,d,e)->f)->a->b->c->d->e->f
curry5 f a b c d e = f (a, b, c, d, e)
curry6 :: ((a,b,c,d,e,f)->g)->a->b->c->d->e->f->g
curry6 g a b c d e f = g (a, b, c, d, e, f)

digest :: (HashFunction hf, ToByteString a) => PlainText hf a -> Digest hf a
digest (PlainText m :: PlainText hf a) = computeDigest m
computeDigest :: (ToByteString a, HashFunction hf) => a -> Digest hf a
computeDigest a = hashFunction (toByteString a)
digestRef :: (ToByteString a, HashFunction hf) => a -> DigestRef hf a
digestRef x = DigestRef x (computeDigest x)
lookupDigest :: (HashFunction hf) => Digest hf a -> a
lookupDigest = traceError "Cannot get a value from its digest"
castDigest :: Digest hf a -> Digest hf b
castDigest (Digest x) = Digest x

trace':: (Show s) => s -> e -> e
trace' s e = trace (show s) e

{-
trace1 :: (Show s, Show a, Show r) => s -> (a -> r) -> a -> r
trace1 s f a = trace (">> " ++ show s ++ " " ++ show a) $
  let r = f a in trace ("<< " ++ show s ++ " " ++ show r) r
trace2 :: (Show s, Show a, Show b, Show r) => s -> (a -> b -> r) -> a -> b -> r
trace2 s f a b = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b) $
  let r = f a b in trace ("<< " ++ show s ++ " " ++ show r) r
trace3 :: (Show s, Show a, Show b, Show c, Show r) => s -> (a -> b -> c -> r) -> a -> b -> c -> r
trace3 s f a b c = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c) $
  let r = f a b c in trace ("<< " ++ show s ++ " " ++ show r) r
etrace1 :: (Monad e, Show s, Show a, Show r) => s -> (a -> e r) -> a -> e r
etrace1 s f a = trace (">> " ++ show s ++ " " ++ show a) $
  f a >>= \ r -> trace ("<< " ++ show s ++ " " ++ show r) $ return r
etrace2 :: (Monad e, Show s, Show a, Show b, Show r) => s -> (a -> b -> e r) -> a -> b -> e r
etrace2 s f a b = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b) $
  f a b >>= \ r -> trace ("<< " ++ show s ++ " " ++ show r) $ return r
etrace3 :: (Monad e, Show s, Show a, Show b, Show c, Show r) => s -> (a -> b -> c -> e r) -> a -> b -> c -> e r
etrace3 s f a b c = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c) $
  f a b c >>= \ r -> trace ("<< " ++ show s ++ " " ++ show r) $ return r
-}

{-
serialiseData :: BuiltinData -> BuiltinByteString
-}

-- * Meta declarations
{-
PlutusTx.makeLift ''FixedLengthByteString
PlutusTx.makeIsDataSchemaIndexed ''FixedLengthByteString [('FixedLengthByteString, 0)]
PlutusTx.makeLift ''Digest
PlutusTx.makeIsDataSchemaIndexed ''Digest [('Digest, 0)]
-}
