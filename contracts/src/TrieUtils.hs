{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- (trace')
module TrieUtils where
--module MyTrieUtils (PreWrapping, Wrapping, LiftRef (..), LiftBinary, LiftShow, LiftEq, DigestibleRef, DigestRef (..), Blake2b_256_Ref, HashAlgorithmOf, wrap, unwrap, integerLength, fbIntegerLength, fbuIntegerLength, lowestBitSet, lowestBitClear, fbLowestBitSet, fbLowestBitClear, fbuLowestBitClear, lowBitsMask, extractBitField, lookupDigest, liftEq, liftShow, liftGet, liftPut, getDigest, digestiblePut, computeDigest, trace', trace1, trace2, trace3, etrace1, etrace2, etrace3) where
-- DigestOnly (..),

import PlutusTx.Prelude
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Show
import PlutusTx.Utils

import Control.Monad (Monad)
-- import Codec.Serialise (serialise, deserialise)
-- import Codec.Serialise.Class (Serialise, encode, decode, encodeList, decodeList)
-- import Codec.Serialise.Encoding (Encoding)
-- import Codec.Serialise.Decoding (Decoder)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (fromJust)

-- ||| Basic Type Functionality

-- | Partial types
class Partial a where
  isElement :: a -> Bool
  validate :: a -> a
  validate a = if isElement a then a else mustBeReplaced "Bad value"

-- | Static Dimensions
class StaticLength i where
  staticLength :: Integer
data L32
instance StaticLength L32 where
  staticLength = 32


-- ||| ByteString of known length or not
data
  (StaticLength len) =>
  FixedLengthByteString len = FixedLengthByteString BuiltinByteString
instance
  (StaticLength len) =>
  Partial (FixedLengthByteString len) where
  isElement (FixedLengthByteString b) = lengthOfByteString b == staticLength @len
instance Eq (FixedLengthByteString len) where
  (FixedLengthByteString x) == (FixedLengthByteString y) = x == y

-- NB: To fit on-chain on Cardano (or affordably on any L1,
-- and thus on any L2 that gets verified by a L2),
-- a VariableLengthByteString has to be of length <= 65535
-- For larger data structures... put them in a Trie wherein only a logarithmic fragment is witnessed
data VariableLengthByteString = VariableLengthByteString BuiltinByteString
instance Partial VariableLengthByteString where
  isElement (VariableLengthByteString b) = lengthOfByteString b <= 65535
instance Eq VariableLengthByteString where
  (VariableLengthByteString x) == (VariableLengthByteString y) = x == y


-- ||| Numbers

-- | Exponentials... how is this not in Plutus already?
multiplyByExponential :: Integer -> Integer -> Integer -> Integer
multiplyByExponential a e n =
  if n == 0 then a else
    let a' = if n `modulo` 2 == 1 then a * e else a
        e' = e * e
        n' = n `divide` 2 in
    multiplyByExponential a' e' n'

exponential :: Integer -> Integer -> Integer
exponential = multiplyByExponential 1

-- | Byte
data Byte = Byte { fromByte :: Integer }
instance Partial Byte where
  isElement (Byte b) = 0 <= b && b <= 255
toByte :: Integer -> Byte
toByte n = validate (Byte n)
instance Eq Byte where
  (Byte x) == (Byte y) = x == y

-- | UInt16
data UInt16 = UInt16 { fromUInt16 :: Integer }
instance Partial UInt16 where
  isElement (UInt16 b) = 0 <= b && b <= 65535
toUInt16 :: Integer -> UInt16
toUInt16 n = validate (UInt16 n)
instance Eq UInt16 where
  (UInt16 x) == (UInt16 y) = x == y

-- | FixedLengthInteger
data
  (StaticLength len) =>
  FixedLengthInteger len = FixedLengthInteger Integer
instance
  (StaticLength len) =>
  Partial (FixedLengthInteger len) where
  isElement (FixedLengthInteger i) = i < exponential 2 (staticLength @len)
  validate f@(FixedLengthInteger i) =
    let b = integerToByteString BigEndian (staticLength @len) i in
      if b == b then f else mustBeReplaced "Bad integer"
instance Eq (FixedLengthInteger len) where
  (FixedLengthInteger x) == (FixedLengthInteger y) = x == y

-- | VariableLengthInteger
data VariableLengthInteger = VariableLengthInteger
  { vliBitLength :: Integer
  , vliInteger :: Integer }
instance Eq VariableLengthInteger where
  x == y = vliInteger x == vliInteger y

-- | toInt
class HasToInt a where
  toInt :: a -> Integer
instance HasToInt Byte where
  toInt = fromByte
instance HasToInt UInt16 where
  toInt = fromUInt16
instance
  HasToInt BuiltinByteString where
  toInt b = byteStringToInteger BigEndian b
instance
  (StaticLength len) =>
  HasToInt (FixedLengthByteString len) where
  toInt (FixedLengthByteString b) = toInt b
instance
  HasToInt VariableLengthByteString where
  toInt (VariableLengthByteString b) = toInt b
instance
  HasToInt (FixedLengthInteger len) where
  toInt (FixedLengthInteger n) = n
instance
  HasToInt VariableLengthInteger where
  toInt = vliInteger

-- | fromInt
class HasFromInt a where
  fromInt :: Integer -> a
instance HasFromInt Byte where
  fromInt = toByte
instance HasFromInt UInt16 where
  fromInt = toUInt16
instance
  (StaticLength len) =>
  HasFromInt (FixedLengthByteString len) where
  fromInt = FixedLengthByteString . integerToByteString BigEndian (staticLength @len)
instance
  HasFromInt VariableLengthByteString where
  fromInt = VariableLengthByteString . toByteString
instance
  (StaticLength len) =>
  HasFromInt (FixedLengthInteger len) where
  fromInt = validate . FixedLengthInteger
instance
  HasFromInt VariableLengthInteger where
  fromInt n = VariableLengthInteger (bitLength n) n

-- | toByteString
class HasToByteString a where
  toByteString :: a -> BuiltinByteString
instance HasToByteString Byte where
  toByteString (Byte n) = integerToByteString BigEndian 1 n
instance HasToByteString UInt16 where
  toByteString (UInt16 n) = integerToByteString BigEndian 2 n
instance HasToByteString Integer where
  toByteString n = integerToByteString BigEndian (bitLengthToByteLength $ bitLength n) n
instance HasToByteString BuiltinByteString where
  toByteString x = x
instance
  (StaticLength len) =>
  HasToByteString (FixedLengthByteString len) where
  toByteString (FixedLengthByteString b) = b
instance
  HasToByteString VariableLengthByteString where
  toByteString (VariableLengthByteString b) = b
instance
  (StaticLength len) =>
  HasToByteString (FixedLengthInteger len) where
  toByteString (FixedLengthInteger n) = integerToByteString BigEndian (staticLength @len) n
instance
  HasToByteString VariableLengthInteger where
  toByteString (VariableLengthInteger l n) = integerToByteString BigEndian (bitLengthToByteLength l) n

bitLengthToByteLength :: Integer -> Integer
bitLengthToByteLength n = (n + 7) `divide` 8

-- | fromByteString
class HasFromByteString a where
  fromByteString :: BuiltinByteString -> a
instance HasFromByteString Byte where
  fromByteString = toByte . byteStringToInteger BigEndian
instance HasFromByteString UInt16 where
  fromByteString = toUInt16 . byteStringToInteger BigEndian
instance HasFromByteString BuiltinByteString where
  fromByteString x = x
instance
  (StaticLength len) =>
  HasFromByteString (FixedLengthByteString len) where
  fromByteString = validate . FixedLengthByteString
instance
  HasFromByteString VariableLengthByteString where
  fromByteString = VariableLengthByteString
instance
  (StaticLength len) =>
  HasFromByteString (FixedLengthInteger len) where
  fromByteString bs =
    if lengthOfByteString bs > staticLength @len then mustBeReplaced "ByteString too long" else
      FixedLengthInteger $ byteStringToInteger BigEndian bs
instance
  HasFromByteString VariableLengthInteger where
  fromByteString b = VariableLengthInteger (bitLength b) (byteStringToInteger BigEndian b)

{- Failure to include bitLength in either Haskell or Plutus seems incompetent to me. --fare
   (see CIP-123, compare to CLHS integer-length) -}
class HasBitLogic a where
  bitLength :: a -> Integer
  lowestBitClear :: a -> Integer -- NB: returns -1 for 0, not maxBitLength, even for non-negative types!
  isBitSet :: Integer -> a -> Bool
  lowBitsMask :: Integer -> a
  logicalOr :: a -> a -> a
  logicalAnd :: a -> a -> a
  shiftRight :: a -> Integer -> a
  shiftLeft :: a -> Integer -> a
bitLength16 :: Integer -> Integer -- assumes input in [0,65535]
bitLength16 n = if n < 256 then bitLength8 n else 8 + bitLength8 (n `divide` 256)
bitLength8 :: Integer -> Integer -- assumes input in [0,255]
bitLength8 n = if n < 16 then bitLength4 n else 4 + bitLength4 (n `divide` 16)
bitLength4 :: Integer -> Integer -- assumes input in [0,15]
bitLength4 n = if n < 4 then bitLength2 n else 2 + bitLength2 (n `divide` 4)
bitLength2 :: Integer -> Integer -- assumes input in [0,3]
bitLength2 n = if n < 3 then n else 2
byteStringFF = integerToByteString BigEndian 1 0xFF
byteStringFFFF = integerToByteString BigEndian 2 0xFFFF
instance HasBitLogic Byte where
  bitLength (Byte n) = bitLength8 n
  lowestBitClear (Byte b) = findFirstSetBit $ toByteString $ 255-b
  isBitSet n b = readBit (toByteString b) n
  lowBitsMask l = Byte $ indexByteString (shiftByteString byteStringFF $ l - 8) 0
  logicalOr a b = Byte $ indexByteString (orByteString False (toByteString a) (toByteString b)) 0
  logicalAnd a b = Byte $ indexByteString (andByteString False (toByteString a) (toByteString b)) 0
  shiftRight b i = Byte $ indexByteString (shiftByteString (toByteString b) $ -i) 0
  shiftLeft b i = Byte $ indexByteString (shiftByteString (toByteString b) i) 0
instance HasBitLogic UInt16 where
  bitLength (UInt16 n) = bitLength16 n
  lowestBitClear (UInt16 b) = findFirstSetBit $ toByteString $ 65535-b
  isBitSet n b = readBit (toByteString b) n
  lowBitsMask l = UInt16 $ indexByteString (shiftByteString byteStringFFFF $ l - 16) 0
  logicalOr a b = UInt16 $ indexByteString (orByteString False (toByteString a) (toByteString b)) 0
  logicalAnd a b = UInt16 $ indexByteString (andByteString False (toByteString a) (toByteString b)) 0
  shiftRight b i = UInt16 $ indexByteString (shiftByteString (toByteString b) $ -i) 0
  shiftLeft b i = UInt16 $ indexByteString (shiftByteString (toByteString b) i) 0
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
  shiftRight b i = b `divide` exponential 2 i
  shiftLeft b i = b * exponential 2 i
equalizeByteStringLength :: BuiltinByteString -> BuiltinByteString -> (BuiltinByteString, BuiltinByteString)
equalizeByteStringLength a b =
  let la = lengthOfByteString a
      lb = lengthOfByteString b in
    if la < lb then (appendByteString (replicateByte 0 $ lb - la) a,b)
    else if la > lb then (a, appendByteString (replicateByte 0 $ la - lb) b)
    else (a, b)
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
  shiftRight b i = shiftByteString b $ -i
  shiftLeft b i = shiftByteString (toByteString b) i
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
  shiftRight (FixedLengthByteString b) i = FixedLengthByteString $ shiftByteString b $ -i
  shiftLeft (FixedLengthByteString b) i = FixedLengthByteString $ shiftByteString (toByteString b) i
instance HasBitLogic VariableLengthByteString where
  bitLength (VariableLengthByteString b) = bitLength b
  lowestBitClear (VariableLengthByteString b) = lowestBitClear b
  isBitSet n (VariableLengthByteString b) = isBitSet n b
  lowBitsMask l = VariableLengthByteString $ lowBitsMask l
  logicalOr (VariableLengthByteString a) (VariableLengthByteString b) =
    VariableLengthByteString $ logicalOr a b
  logicalAnd (VariableLengthByteString a) (VariableLengthByteString b) =
    VariableLengthByteString $ logicalAnd a b
  shiftRight (VariableLengthByteString b) i = VariableLengthByteString $ shiftRight b i
  shiftLeft (VariableLengthByteString b) i = VariableLengthByteString $ shiftLeft b i
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
  shiftRight (FixedLengthInteger b) i = FixedLengthInteger $ shiftRight b i
  shiftLeft (FixedLengthInteger b) i = FixedLengthInteger . toInt $ shiftByteString (toByteString b) i
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
  shiftRight (VariableLengthInteger l b) i = VariableLengthInteger (l - i `max` 0) $ shiftRight b i
  shiftLeft (VariableLengthInteger l b) i = VariableLengthInteger (l + i) $ shiftLeft b i

-- ||| (De)Serializing to(from) bytes

-- | Output to ByteString
class ByteStringOut a where
  byteStringOut :: a -> BuiltinByteString -> BuiltinByteString
toByteStringOut :: ByteStringOut a => a -> BuiltinByteString
toByteStringOut = flip byteStringOut emptyByteString
instance
  (ByteStringOut a, ByteStringOut b) =>
  ByteStringOut (a, b) where
  byteStringOut (a, b) s  = byteStringOut a (byteStringOut b s)
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c) =>
  ByteStringOut (a, b, c) where
  byteStringOut (a, b, c) s  = byteStringOut a (byteStringOut b (byteStringOut c s))
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d) =>
  ByteStringOut (a, b, c, d) where
  byteStringOut (a, b, c, d) s  = byteStringOut a (byteStringOut b (byteStringOut c (byteStringOut d s)))
instance
  (ByteStringOut a, ByteStringOut b, ByteStringOut c, ByteStringOut d, ByteStringOut e) =>
  ByteStringOut (a, b, c, d, e) where
  byteStringOut (a, b, c, d, e) s  = byteStringOut a (byteStringOut b (byteStringOut c (byteStringOut d (byteStringOut e s))))
instance
  (ByteStringOut a) =>
  ByteStringOut [a] where -- length limit 65535
  byteStringOut l s = byteStringOut (toUInt16 $ length l) $ foldr byteStringOut s l
instance ByteStringOut Byte where
  byteStringOut (Byte b) = consByteString b
instance ByteStringOut UInt16 where
  byteStringOut = appendByteString . toByteString
instance ByteStringOut Integer where
  byteStringOut = byteStringOut . toByteString
instance ByteStringOut BuiltinByteString where
  byteStringOut b s =
    let len = toUInt16 $ lengthOfByteString b in
    appendByteString (toByteString len) $ appendByteString b s
instance ByteStringOut (FixedLengthByteString len) where
  byteStringOut (FixedLengthByteString b) = appendByteString b
instance ByteStringOut VariableLengthByteString where
  byteStringOut (VariableLengthByteString b) = byteStringOut b
instance
  (StaticLength len) =>
  ByteStringOut (FixedLengthInteger len) where
  byteStringOut (FixedLengthInteger i) =
    appendByteString (integerToByteString BigEndian (staticLength @len) i)
instance ByteStringOut VariableLengthInteger where
  byteStringOut = byteStringOut . toByteString


-- | Pure Input from ByteString
class ByteStringIn a where
  byteStringIn :: ByteStringCursor -> Maybe (a, ByteStringCursor)
fromByteStringIn :: ByteStringIn a => BuiltinByteString -> Maybe a
fromByteStringIn bs = byteStringCursor bs & byteStringIn >>= \ (a, bs') ->
  if emptyByteStringCursor bs' then Just a else Nothing
data ByteStringCursor = ByteStringCursor
  { cursorByteString :: BuiltinByteString -- the bytes
  , cursorStart :: Integer -- start index included
  , cursorEnd :: Integer } -- end index not included
byteStringCursor :: BuiltinByteString -> ByteStringCursor
byteStringCursor bs = ByteStringCursor bs 0 (lengthOfByteString bs)
emptyByteStringCursor :: ByteStringCursor -> Bool
emptyByteStringCursor bsc = cursorStart bsc >= cursorEnd bsc
nextByteStringCursor :: ByteStringCursor -> Maybe (Byte, ByteStringCursor)
nextByteStringCursor bsc =
  if emptyByteStringCursor bsc then Nothing else
    Just (Byte $ indexByteString (cursorByteString bsc) (cursorStart bsc),
          ByteStringCursor (cursorByteString bsc) (cursorStart bsc + 1) (cursorEnd bsc))
instance
  (ByteStringIn a, ByteStringIn b) =>
  ByteStringIn (a, b) where
  byteStringIn s = byteStringIn s >>= \ (a, s') ->
             byteStringIn s' <&> \ (b, s'') ->
             ((a, b), s'')
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c) =>
  ByteStringIn (a, b, c) where
  byteStringIn s = byteStringIn s >>= \ (a, s') ->
             byteStringIn s' >>= \ (b, s'') ->
             byteStringIn s'' <&> \ (c, s''') ->
             ((a, b, c), s''')
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c, ByteStringIn d) =>
  ByteStringIn (a, b, c, d) where
  byteStringIn s = byteStringIn s >>= \ (a, s') ->
             byteStringIn s' >>= \ (b, s'') ->
             byteStringIn s'' >>= \ (c, s''') ->
             byteStringIn s''' <&> \ (d, s'''') ->
             ((a, b, c, d), s'''')
instance
  (ByteStringIn a, ByteStringIn b, ByteStringIn c, ByteStringIn d, ByteStringIn e) =>
  ByteStringIn (a, b, c, d, e) where
  byteStringIn s = byteStringIn s >>= \ (a, s') ->
             byteStringIn s' >>= \ (b, s'') ->
             byteStringIn s'' >>= \ (c, s''') ->
             byteStringIn s''' >>= \ (d, s'''') ->
             byteStringIn s'''' <&> \ (e, s''''') ->
             ((a, b, c, d, e), s''''')
instance
  (ByteStringIn a) =>
  ByteStringIn [a] where -- length limit 65535
  byteStringIn s =
    byteStringIn s >>= \ (UInt16 len, s') -> loop len s' where
      loop n s'' = if n == 0 then Just ([], s'') else
        byteStringIn s'' >>= \ (a, s''') -> loop (n - 1) s''' <&> \ (l, s'''') -> ((a : l), s'''')

instance ByteStringIn Byte where
  byteStringIn = nextByteStringCursor
instance ByteStringIn UInt16 where
  byteStringIn s = nextByteStringCursor s >>= \ (Byte hi, s') ->
             nextByteStringCursor s' <&> \ (Byte lo, s'') ->
             (UInt16 (hi * 256 + lo), s'')
byteStringInByteString :: Integer -> ByteStringCursor -> Maybe (BuiltinByteString, ByteStringCursor)
byteStringInByteString n bsc =
  let next = cursorStart bsc + n in
    if next <= cursorEnd bsc then
      Just (sliceByteString (cursorStart bsc) n (cursorByteString bsc),
            ByteStringCursor (cursorByteString bsc) next (cursorEnd bsc))
    else Nothing
instance
  (StaticLength len) =>
  ByteStringIn (FixedLengthByteString len) where
  byteStringIn s =
    byteStringInByteString (staticLength @len) s <&> \ (b, s') ->
    (FixedLengthByteString b, s')
instance ByteStringIn VariableLengthByteString where
  byteStringIn s =
    byteStringIn s >>= \ (UInt16 len, s') ->
    byteStringInByteString len s' <&> \ (b, s'') ->
    (VariableLengthByteString b, s'')
instance
  (StaticLength len) =>
  ByteStringIn (FixedLengthInteger len) where
  byteStringIn s =
    byteStringInByteString (staticLength @len) s <&> \ (b, s') ->
    (FixedLengthInteger $ byteStringToInteger BigEndian b, s')



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

instance LiftEq Identity where
  liftEq x y = (runIdentity x) == (runIdentity y)

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

class
  (StaticLength (HashLength hf)) =>
  HashFunction hf where
  type HashLength hf
  hashFunction :: BuiltinByteString -> Digest hf

data Digest hf = Digest { digestByteString :: FixedLengthByteString (HashLength hf) }
instance
  (HashFunction hf) =>
  Eq (Digest hf) where
  (Digest x) == (Digest y) = x == y
instance
  (HashFunction hf) =>
  ByteStringIn (Digest hf) where
  byteStringIn s = byteStringIn s <&> \ (b, s') -> (Digest b, s)
instance
  (HashFunction hf) =>
  ByteStringOut (Digest hf) where
  byteStringOut (Digest b) = byteStringOut b

data Message hf = Message BuiltinByteString

digest :: HashFunction hf => Message hf -> Digest hf
digest (Message m :: Message hf) = hashFunction @hf m

data Blake2b_256
instance HashFunction Blake2b_256 where
  type HashLength Blake2b_256 = L32
  hashFunction = Digest . FixedLengthByteString . blake2b_256

computeDigest :: (ByteStringOut a, HashFunction hf) => a -> Digest hf
computeDigest x = -- digest ((Message (toByteStringOut x)) :: Message hf)
  hashFunction (toByteStringOut x)

class
  (HashFunction hf) =>
  DigestibleRef hf r
  where
  getDigest :: r a -> Digest hf

data DigestRef hf x = DigestRef {digestRefValue :: x, digestRefDigest :: Digest hf}

-- instance LiftShow (DigestRef hf) where
--   liftShow = show . digestRefDigest

instance
  (HashFunction hf) =>
  Eq (DigestRef hf x) where
  (DigestRef _ ah) == (DigestRef _ bh) = ah == bh

instance (HashFunction hf) => ByteStringOut (DigestRef hf x) where
   byteStringOut = byteStringOut . digestRefDigest
{-
instance (HashFunction hf) => ByteStringIn (DigestRef hf x) where
    byteStringIn s = byteStringIn s >>= \ (b, s') -> Just (lookupDigest b, s') -}

instance DigestibleRef Blake2b_256 Blake2b_256_Ref where
  getDigest = digestRefDigest

type Blake2b_256_Ref = DigestRef Blake2b_256

instance (ByteStringOut a, HashFunction hf) => PreWrapping a (LiftRef (DigestRef hf)) Identity where
  wrap x = Identity (LiftRef $ DigestRef x $ (computeDigest @a @hf x))

instance (ByteStringOut a, HashFunction hf) => Wrapping a (LiftRef (DigestRef hf)) Identity where
  unwrap (LiftRef (DigestRef x _)) = Identity x

lookupDigest :: (HashFunction hf) => Digest hf -> a
lookupDigest = mustBeReplaced "Cannot get a value from its digest"

{-
-- TODO: more efficient implementation?
-- First argument is the length, second is the start (little endian), last is the bits
extractBitField :: (Bits n, Integral n) => Int -> Int -> n -> n
extractBitField len start bits =
  (bits `shiftR` start) .&. (lowBitsMask len)
-}

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


-- ||| Reference

-- | Lifting properties from a reference

class LiftByteStringOut r where
  liftByteStringOut :: (ByteStringOut a) => r a -> BuiltinByteString -> BuiltinByteString

class LiftByteStringIn r where
  liftByteStringIn :: (ByteStringIn a) => BuiltinByteString -> Maybe (r a, BuiltinByteString)

class LiftEq r where
  liftEq :: (Eq a) => r a -> r a -> Bool

class LiftShow r where
  liftShow :: (Show a) => r a -> BuiltinString
instance LiftShow Identity where
  liftShow = show . runIdentity

-- | Datum
class
  (ByteStringOut d, Show d, Eq d) =>
  Datum d
class
  (LiftByteStringOut r, LiftShow r, LiftEq r) =>
  LiftDatum r

-- | Y-combinator or fixed point combinator for types
newtype Fix f = In {out :: f (Fix f)}
instance
  (LiftByteStringOut f) =>
  ByteStringOut (Fix f)
  where
  byteStringOut (In x) = liftByteStringOut x
instance
  (LiftEq f) =>
  Eq (Fix f)
  where
  (==) x y = liftEq (out x) (out y)
instance
  (LiftShow f) =>
  Show (Fix f)
  where
  show = liftShow . out
instance
  (LiftDatum r) =>
  Datum (Fix r)
  where

-- | LiftRef
data LiftRef r a = LiftRef {liftref :: r a}
  deriving (Functor)
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
  (LiftDatum r, Datum a) =>
  Datum (LiftRef r a)
  where

instance
  (LiftByteStringOut r, ByteStringOut a) =>
  ByteStringOut (LiftRef r a)
  where
  byteStringOut = liftByteStringOut . liftref

-- | PreWrapping : a value can be wrapped (but maybe not unwrapped!)
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
