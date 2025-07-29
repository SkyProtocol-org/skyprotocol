{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Types.Base where

import Control.Monad (Monad, (>=>))
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.String (IsString, String, fromString)
import Data.Text (pack, unpack)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx as P
import PlutusTx.Blueprint as P
import PlutusTx.Builtins as P
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude as P
import PlutusTx.Show as P
import Text.Hex (decodeHex, encodeHex)
import Prelude qualified as HP

-- * Types

type BBS = P.BuiltinByteString

-- | Redefining 'Proxy', since Plutus doesn't support `k :: t`
-- e.g. type of type thingy
data Proxy a = Proxy

-- | Backwards composition
(-.) :: (a -> b) -> (b -> c) -> a -> c
(-.) = flip (.)

-- | Byte
newtype Byte = Byte {getByte :: Integer}
  deriving (HP.Eq, P.Eq, ToInt) via Integer
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

-- | UInt16
newtype UInt16 = UInt16 {getUInt16 :: Integer}
  deriving (HP.Eq, P.Eq, ToInt) via Integer
  deriving stock (Generic)
  deriving newtype (HP.Show, P.Show)
  deriving anyclass (P.HasBlueprintDefinition)

-- | VariableLengthInteger
data VariableLengthInteger = VariableLengthInteger
  { vliBitLength :: Integer,
    vliInteger :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

-- | Pure Input from ByteString
newtype ByteStringReader a = ByteStringReader
  {getByteStringReader :: ByteStringCursor -> Maybe (a, ByteStringCursor)}

data ByteStringCursor = ByteStringCursor
  { cursorByteString :: P.BuiltinByteString, -- the bytes
    cursorStart :: Integer, -- start index included
    cursorEnd :: Integer -- end index not included
  }
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

data IsTerminal = NonTerminal | Terminal

-- ** Fixed size data structures

newtype Bytes4 = Bytes4 {getBytes4 :: BBS}
  deriving (HP.Eq, P.Eq, ToInt) via BBS
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

newtype Bytes8 = Bytes8 {getBytes8 :: BBS}
  deriving (HP.Eq, P.Eq, ToInt) via BBS
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

newtype Bytes32 = Bytes32 {getBytes32 :: BBS}
  deriving (HP.Eq, P.Eq, ToInt) via BBS
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

newtype Bytes64 = Bytes64 {getBytes64 :: BBS}
  deriving (HP.Eq, P.Eq, ToInt) via BBS
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

newtype UInt32 = UInt32 {getUInt32 :: Integer}
  deriving (HP.Eq, P.Eq, ToInt) via Integer
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

newtype UInt64 = UInt64 {getUInt64 :: Integer}
  deriving (HP.Eq, P.Eq, ToInt) via Integer
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

newtype UInt256 = UInt256 {getUInt256 :: Integer}
  deriving (HP.Eq, P.Eq, ToInt) via Integer
  deriving newtype (HP.Show, P.Show)
  deriving stock (Generic)
  deriving anyclass (P.HasBlueprintDefinition)

-- | Wrapper for Isomorphic derivation
-- See https://www.tweag.io/blog/2020-04-23-deriving-isomorphically/
newtype As a b = As {getAs :: b}

-- * Classes

-- | two types are Isomorphic
-- convertTo . convertFrom == id @b
-- convertFrom . convertTo == id @a
-- See https://www.tweag.io/blog/2020-04-23-deriving-isomorphically/
type Isomorphic a b = (ConvertTo a b, ConvertFrom a b)

class ConvertTo a b where
  convertTo :: a -> b

class ConvertFrom a b where
  convertFrom :: b -> a

-- | Partial types
class Partial a where
  isElement :: a -> P.Bool

  -- | given an element of the type, return Just it if it is well-formed, Nothing otherwise.
  maybeValidate :: a -> Maybe a
  maybeValidate a = if isElement a then Just a else Nothing

  -- | partial function! Will fail if the element is not an element
  validate :: a -> a
  validate = fromJust . maybeValidate

  {-# MINIMAL isElement #-}

-- | 'toInt' is unsafe if 'maybeToInt' is partial.
class ToInt a where
  toInt :: a -> Integer
  toInt = fromJust . maybeToInt

  maybeToInt :: a -> Maybe Integer
  maybeToInt = Just . toInt

  {-# MINIMAL toInt | maybeToInt #-}

-- | fromInt is not safe unless the function is total, will error out on bad input
-- Use with care.
class FromInt a where
  fromInt :: Integer -> a
  fromInt = fromJust . maybeFromInt

  maybeFromInt :: Integer -> Maybe a
  maybeFromInt = Just . fromInt

  {-# MINIMAL fromInt | maybeFromInt #-}

-- | At least one of toByteString or appendByteStringTerminal must be defined.
-- Additionally, if the output isn't fixed-length or otherwise self-delimited,
-- then byteStringOut must be defined.
-- TODO: replace with something that uses Cardano's standard CBOR encoding.
-- But keep the bytestring variant for the sake of other chains?
class ToByteString a where
  -- | convert to String
  toByteString :: a -> P.BuiltinByteString
  toByteString = toByteStringOut

  -- | append to a String in Terminal or NonTerminal position
  -- default method assumes self-delimitation (e.g. fixed length, length prefix, or terminator)
  byteStringOut :: a -> IsTerminal -> P.BuiltinByteString -> P.BuiltinByteString
  byteStringOut a _ = appendByteString $ toByteString a

-- | fromByteString is almost always unsafe, unless every byte string of any length is valid.
class FromByteString a where
  {-# INLINEABLE fromByteString #-}
  fromByteString :: P.BuiltinByteString -> a
  fromByteString = fromByteStringIn

  --  fromByteString = fromJust . maybeFromByteString
  byteStringIn :: IsTerminal -> ByteStringReader a

  --  ??? THIS CAUSES HAVOC IN THE PLUTUS COMPILER, WHY???
  --  {-# INLINEABLE maybeFromByteString__ #-}
  --  maybeFromByteString__ :: BuiltinByteString -> Maybe a
  --  maybeFromByteString__ = maybeFromByteStringIn

  {-# MINIMAL byteStringIn #-}

{- Failure to include bitLength in either Haskell or Plutus seems incompetent to me. --fare
   (see CIP-123, compare to CLHS integer-length) -}
class BitLogic a where
  bitLength :: a -> Integer
  lowestBitClear ::
    a ->
    -- | NB: returns nBits for allBits, but -1 for -1 for Integers.
    Integer
  isBitSet :: Integer -> a -> Bool
  lowBitsMask :: Integer -> a
  logicalOr :: a -> a -> a
  logicalAnd :: a -> a -> a
  logicalXor :: a -> a -> a
  shiftRight :: a -> Integer -> a
  shiftLeft :: a -> Integer -> a
  shiftLeftWithBits :: a -> Integer -> a -> a
  shiftLeftWithBits a l b = (a `shiftLeft` l) `logicalOr` b

type Dato d =
  ( ToByteString d,
    FromByteString d,
    P.ToData d,
    P.FromData d,
    P.UnsafeFromData d,
    P.Show d,
    P.Eq d
  )

-- * Instances

-- ** Bytes4

instance Partial Bytes4 where
  isElement = isBBSLength 4 . getBytes4

instance FromInt Bytes4 where
  maybeFromInt = fmap Bytes4 . maybeFLBBSFromInt 4

instance ToByteString Bytes4 where
  toByteString = getBytes4
  byteStringOut (Bytes4 b) _ = appendByteString b

instance FromByteString Bytes4 where
  -- {-# INLINEABLE fromByteString #-}
  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeValidate . Bytes4
  byteStringIn _ = byteStringInFixedLength 4 <&> Bytes4

instance BitLogic Bytes4 where
  bitLength = bitLength . getBytes4
  lowestBitClear = lowestBitClear . getBytes4
  isBitSet n (Bytes4 b) = readBit b n
  lowBitsMask = Bytes4 . lowBitsMaskFLBBS 4
  logicalOr (Bytes4 a) (Bytes4 b) = Bytes4 $ orByteString False a b
  logicalAnd (Bytes4 a) (Bytes4 b) = Bytes4 $ andByteString False a b
  logicalXor (Bytes4 a) (Bytes4 b) = Bytes4 $ xorByteString False a b
  shiftRight (Bytes4 b) l = Bytes4 $ shiftRightFLBBS 4 b l
  shiftLeft (Bytes4 b) l = Bytes4 $ shiftLeftFLBBS 4 b l

-- ** Bytes8

instance Partial Bytes8 where
  isElement = isBBSLength 8 . getBytes8

instance FromInt Bytes8 where
  maybeFromInt = fmap Bytes8 . maybeFLBBSFromInt 8

instance ToByteString Bytes8 where
  toByteString = getBytes8
  byteStringOut (Bytes8 b) _ = appendByteString b

instance FromByteString Bytes8 where
  -- {-# INLINEABLE fromByteString #-}
  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeValidate . FixedLengthByteString
  byteStringIn _ = byteStringInFixedLength 8 <&> Bytes8

instance BitLogic Bytes8 where
  bitLength = bitLength . getBytes8
  lowestBitClear = lowestBitClear . getBytes8
  isBitSet n (Bytes8 b) = readBit b n
  lowBitsMask = Bytes8 . lowBitsMaskFLBBS 8
  logicalOr (Bytes8 a) (Bytes8 b) = Bytes8 $ orByteString False a b
  logicalAnd (Bytes8 a) (Bytes8 b) = Bytes8 $ andByteString False a b
  logicalXor (Bytes8 a) (Bytes8 b) = Bytes8 $ xorByteString False a b
  shiftRight (Bytes8 b) l = Bytes8 $ shiftRightFLBBS 8 b l
  shiftLeft (Bytes8 b) l = Bytes8 $ shiftLeftFLBBS 8 b l

-- ** Bytes32

instance Partial Bytes32 where
  isElement = isBBSLength 32 . getBytes32

instance FromInt Bytes32 where
  maybeFromInt = fmap Bytes32 . maybeFLBBSFromInt 32

instance ToByteString Bytes32 where
  toByteString = getBytes32
  byteStringOut (Bytes32 b) _ = appendByteString b

instance FromByteString Bytes32 where
  -- {-# INLINEABLE fromByteString #-}
  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeValidate . FixedLengthByteString
  byteStringIn _ = byteStringInFixedLength 32 <&> Bytes32

instance BitLogic Bytes32 where
  bitLength = bitLength . getBytes32
  lowestBitClear = lowestBitClear . getBytes32
  isBitSet n (Bytes32 b) = readBit b n
  lowBitsMask = Bytes32 . lowBitsMaskFLBBS 32
  logicalOr (Bytes32 a) (Bytes32 b) = Bytes32 $ orByteString False a b
  logicalAnd (Bytes32 a) (Bytes32 b) = Bytes32 $ andByteString False a b
  logicalXor (Bytes32 a) (Bytes32 b) = Bytes32 $ xorByteString False a b
  shiftRight (Bytes32 b) l = Bytes32 $ shiftRightFLBBS 32 b l
  shiftLeft (Bytes32 b) l = Bytes32 $ shiftLeftFLBBS 32 b l

-- ** Bytes64

instance Partial Bytes64 where
  isElement = isBBSLength 64 . getBytes64

instance FromInt Bytes64 where
  maybeFromInt = fmap Bytes64 . maybeFLBBSFromInt 64

instance ToByteString Bytes64 where
  toByteString = getBytes64
  byteStringOut (Bytes64 b) _ = appendByteString b

instance FromByteString Bytes64 where
  -- {-# INLINEABLE fromByteString #-}
  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeValidate . FixedLengthByteString
  byteStringIn _ = byteStringInFixedLength 64 <&> Bytes64

instance BitLogic Bytes64 where
  bitLength = bitLength . getBytes64
  lowestBitClear = lowestBitClear . getBytes64
  isBitSet n (Bytes64 b) = readBit b n
  lowBitsMask = Bytes64 . lowBitsMaskFLBBS 64
  logicalOr (Bytes64 a) (Bytes64 b) = Bytes64 $ orByteString False a b
  logicalAnd (Bytes64 a) (Bytes64 b) = Bytes64 $ andByteString False a b
  logicalXor (Bytes64 a) (Bytes64 b) = Bytes64 $ xorByteString False a b
  shiftRight (Bytes64 b) l = Bytes64 $ shiftRightFLBBS 64 b l
  shiftLeft (Bytes64 b) l = Bytes64 $ shiftLeftFLBBS 64 b l

-- ** BuiltinByteString

-- NB: To fit on-chain on Cardano (or affordably on any L1,
-- and thus on any L2 that gets verified by a L2),
-- a BuiltinByteString has to be of length <= 65535 (in practice smaller, more like 8192)
-- For larger data structures... put them in a Trie wherein only a logarithmic fragment is witnessed

instance Partial P.BuiltinByteString where
  isElement b = lengthOfByteString b <= 65535

instance ToInt P.BuiltinByteString where
  toInt = fromByteString

instance FromInt P.BuiltinByteString where
  fromInt = toByteString

instance ToByteString P.BuiltinByteString where
  toByteString = id
  byteStringOut b Terminal = appendByteString b
  byteStringOut b NonTerminal =
    let len = toUInt16 $ lengthOfByteString b
     in appendByteString (toByteString len) . appendByteString b

instance FromByteString P.BuiltinByteString where
  fromByteString = id
  byteStringIn Terminal = byteStringInToEnd
  byteStringIn NonTerminal = byteStringIn NonTerminal >>= \(UInt16 len) -> byteStringInFixedLength len

instance BitLogic P.BuiltinByteString where
  bitLength b =
    let len = lengthOfByteString b
        loop i =
          if i >= len
            then 0
            else
              let byte = indexByteString b i
               in if byte > 0 then bitLength8 byte + 8 * (len - i - 1) else loop $ i + 1
     in loop 0
  lowestBitClear b =
    let n = findFirstSetBit $ complementByteString b
     in if n == -1 then 8 * lengthOfByteString b else n
  isBitSet = flip readBit
  lowBitsMask l = shiftByteString (replicateByte (bitLengthToByteLength l) 0xFF) $ (-l) `quotient` 8
  logicalOr a b = let (a', b') = equalizeByteStringLength a b in orByteString False a' b'
  logicalAnd a b = let (a', b') = equalizeByteStringLength a b in andByteString False a' b'
  logicalXor a b = let (a', b') = equalizeByteStringLength a b in xorByteString False a' b'
  shiftRight b i = shiftByteString b $ -i
  shiftLeft b = shiftByteString (toByteString b)

-- ** BuiltinString

instance ToByteString P.BuiltinString where
  toByteString = encodeUtf8
  byteStringOut = byteStringOut . toByteString

instance FromByteString P.BuiltinString where
  fromByteString = decodeUtf8 -- XXX
  byteStringIn isTerminal = byteStringIn isTerminal <&> decodeUtf8

-- looks like nonsense instances?
-- upd: instances for testing (why?)
instance P.ToData P.BuiltinString where
  toBuiltinData = toBuiltinData . toByteString

instance P.FromData P.BuiltinString where
  fromBuiltinData d = fromBuiltinData d >>= maybeFromByteString

instance P.UnsafeFromData P.BuiltinString where
  unsafeFromBuiltinData = fromByteString . unsafeFromBuiltinData

-- ** Byte

instance Partial Byte where
  isElement = isUInt 8 . getByte

instance FromInt Byte where
  maybeFromInt = maybeValidate . Byte

instance ToByteString Byte where
  toByteString (Byte n) = integerToByteString BigEndian 1 n
  byteStringOut (Byte n) _ = consByteString n

instance FromByteString Byte where
  -- {-# INLINEABLE fromByteString #-} -- XXX
  fromByteString = Byte . byteStringToInteger BigEndian . validateFLBBS 1

  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeFromByteString >=>
  --  return . Byte . byteStringToInteger BigEndian . toByteString @(FixedLengthByteString L1)
  byteStringIn _ = ByteStringReader nextByteStringCursor

instance BitLogic Byte where
  bitLength (Byte n) = bitLength8 n
  lowestBitClear (Byte b) =
    let n = findFirstSetBit $ toByteString $ 255 - b
     in if n == -1 then 8 else n
  isBitSet n b = readBit (toByteString b) n
  lowBitsMask l = Byte $ indexByteString (shiftByteString byteString1 $ l - 8) 0
  logicalOr a b = Byte $ indexByteString (orByteString False (toByteString a) (toByteString b)) 0
  logicalAnd a b = Byte $ indexByteString (andByteString False (toByteString a) (toByteString b)) 0
  logicalXor a b = Byte $ indexByteString (xorByteString False (toByteString a) (toByteString b)) 0
  shiftRight b i = Byte $ indexByteString (shiftByteString (toByteString b) $ -i) 0
  shiftLeft b i = Byte $ indexByteString (shiftByteString (toByteString b) i) 0
  shiftLeftWithBits (Byte a) l (Byte b) = Byte $ (a `shiftLeft` l) + b

-- ** UInt16

instance Partial UInt16 where
  isElement = isUInt 16 . getUInt16

instance FromInt UInt16 where
  -- fromInt n = UInt16 (n `remainder` 65536) -- DEBUG
  maybeFromInt = maybeValidate . UInt16

instance ToByteString UInt16 where
  toByteString (UInt16 n) = integerToByteString BigEndian 2 n

instance FromByteString UInt16 where
  -- {-# INLINEABLE fromByteString #-}
  fromByteString = UInt16 . byteStringToInteger BigEndian . validateFLBBS 2

  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeFromByteString >=>
  --  return . UInt16 . byteStringToInteger BigEndian . toByteString @(FixedLengthByteString L2)
  byteStringIn _ = byteStringInFixedLength 2 <&> fromByteString

instance BitLogic UInt16 where
  bitLength (UInt16 n) = bitLength16 n
  lowestBitClear (UInt16 b) =
    let n = findFirstSetBit $ toByteString $ 65535 - b
     in if n == -1 then 16 else n
  isBitSet n b = readBit (toByteString b) n
  lowBitsMask l = UInt16 $ indexByteString (shiftByteString byteString2 $ l - 16) 0
  logicalOr a b = UInt16 $ indexByteString (orByteString False (toByteString a) (toByteString b)) 0
  logicalAnd a b = UInt16 $ indexByteString (andByteString False (toByteString a) (toByteString b)) 0
  logicalXor a b = UInt16 $ indexByteString (xorByteString False (toByteString a) (toByteString b)) 0
  shiftRight b i = UInt16 $ indexByteString (shiftByteString (toByteString b) $ -i) 0
  shiftLeft b i = UInt16 $ indexByteString (shiftByteString (toByteString b) i) 0
  shiftLeftWithBits (UInt16 a) l (UInt16 b) = UInt16 $ (a `shiftLeft` l) + b

-- ** UInt32

instance Partial UInt32 where
  isElement = isUInt 32 . getUInt32

instance FromInt UInt32 where
  maybeFromInt = maybeValidate . UInt32

instance ToByteString UInt32 where
  toByteString (UInt32 n) = integerToByteString BigEndian 4 n

instance FromByteString UInt32 where
  --  {-# INLINEABLE fromByteString #-}
  --  fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  --  maybeFromByteString = maybeValidate . FixedLengthInteger . byteStringToInteger BigEndian
  byteStringIn _ =
    byteStringInFixedLength 4
      <&> UInt32
      . byteStringToInteger BigEndian

instance BitLogic UInt32 where
  bitLength = bitLength . integerToByteString BigEndian 4 . getUInt32
  lowestBitClear n = lowestBitClear $ toByteString n
  isBitSet n i = isBitSet n (toByteString i)
  lowBitsMask l = UInt32 . toInt $ lowBitsMaskFLBBS 4 l
  logicalOr (UInt32 a) (UInt32 b) = UInt32 . toInt $ logicalOr (fromInt a :: Bytes4) (fromInt b)
  logicalAnd (UInt32 a) (UInt32 b) = UInt32 . toInt $ logicalAnd (fromInt a :: Bytes4) (fromInt b)
  logicalXor (UInt32 a) (UInt32 b) = UInt32 . toInt $ logicalXor (fromInt a :: Bytes4) (fromInt b)
  shiftRight (UInt32 b) i = UInt32 $ shiftRight b i
  shiftLeft (UInt32 b) i = UInt32 . toInt $ shiftByteString (toByteString b) i
  shiftLeftWithBits (UInt32 a) l (UInt32 b) = UInt32 $ (a `shiftLeft` l) + b

-- ** UInt64

instance Partial UInt64 where
  isElement = isUInt 64 . getUInt64

instance FromInt UInt64 where
  maybeFromInt = maybeValidate . UInt64

instance ToByteString UInt64 where
  toByteString (UInt64 n) = integerToByteString BigEndian 8 n

instance FromByteString UInt64 where
  --  {-# INLINEABLE fromByteString #-}
  --  fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  --  maybeFromByteString = maybeValidate . FixedLengthInteger . byteStringToInteger BigEndian
  byteStringIn _ =
    byteStringInFixedLength 8
      <&> UInt64
      . byteStringToInteger BigEndian

instance BitLogic UInt64 where
  bitLength = bitLength . integerToByteString BigEndian 8 . getUInt64
  lowestBitClear n = lowestBitClear $ toByteString n
  isBitSet n i = isBitSet n (toByteString i)
  lowBitsMask l = UInt64 . toInt $ lowBitsMaskFLBBS 8 l
  logicalOr (UInt64 a) (UInt64 b) = UInt64 . toInt $ logicalOr (fromInt a :: Bytes4) (fromInt b)
  logicalAnd (UInt64 a) (UInt64 b) = UInt64 . toInt $ logicalAnd (fromInt a :: Bytes4) (fromInt b)
  logicalXor (UInt64 a) (UInt64 b) = UInt64 . toInt $ logicalXor (fromInt a :: Bytes4) (fromInt b)
  shiftRight (UInt64 b) i = UInt64 $ shiftRight b i
  shiftLeft (UInt64 b) i = UInt64 . toInt $ shiftByteString (toByteString b) i
  shiftLeftWithBits (UInt64 a) l (UInt64 b) = UInt64 $ (a `shiftLeft` l) + b

-- ** UInt256

instance Partial UInt256 where
  isElement = isUInt 256 . getUInt256

instance FromInt UInt256 where
  maybeFromInt = maybeValidate . UInt256

instance ToByteString UInt256 where
  toByteString (UInt256 n) = integerToByteString BigEndian 32 n

instance FromByteString UInt256 where
  --  {-# INLINEABLE fromByteString #-}
  --  fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  --  maybeFromByteString = maybeValidate . FixedLengthInteger . byteStringToInteger BigEndian
  byteStringIn _ =
    byteStringInFixedLength 32
      <&> UInt256
      . byteStringToInteger BigEndian

instance BitLogic UInt256 where
  bitLength = bitLength . integerToByteString BigEndian 32 . getUInt256
  lowestBitClear n = lowestBitClear $ toByteString n
  isBitSet n i = isBitSet n (toByteString i)
  lowBitsMask l = UInt256 . toInt $ lowBitsMaskFLBBS 32 l
  logicalOr (UInt256 a) (UInt256 b) = UInt256 . toInt $ logicalOr (fromInt a :: Bytes4) (fromInt b)
  logicalAnd (UInt256 a) (UInt256 b) = UInt256 . toInt $ logicalAnd (fromInt a :: Bytes4) (fromInt b)
  logicalXor (UInt256 a) (UInt256 b) = UInt256 . toInt $ logicalXor (fromInt a :: Bytes4) (fromInt b)
  shiftRight (UInt256 b) i = UInt256 $ shiftRight b i
  shiftLeft (UInt256 b) i = UInt256 . toInt $ shiftByteString (toByteString b) i
  shiftLeftWithBits (UInt256 a) l (UInt256 b) = UInt256 $ (a `shiftLeft` l) + b

-- ** VariableLengthInteger

{- Limitation:
 - only integers of length less than 65536
 - I/O only for non-negative integers (for now)
 - we do not reject non-canonical input (leading zeros) -}
instance P.Eq VariableLengthInteger where
  x == y = vliInteger x == vliInteger y

instance P.Show VariableLengthInteger where
  showsPrec prec (VariableLengthInteger _ i) =
    showApp prec "toVariableLengthInteger" [showArg i]

instance ToInt VariableLengthInteger where
  toInt = vliInteger

instance FromInt VariableLengthInteger where
  fromInt n = VariableLengthInteger (bitLength n) n

instance ToByteString VariableLengthInteger where
  toByteString (VariableLengthInteger l n) = integerToByteString BigEndian (bitLengthToByteLength l) n
  byteStringOut = byteStringOut . toByteString

instance FromByteString VariableLengthInteger where
  fromByteString b = VariableLengthInteger (bitLength b) (byteStringToInteger BigEndian b)
  byteStringIn isTerminal = byteStringIn isTerminal <&> fromByteString

instance BitLogic VariableLengthInteger where
  bitLength (VariableLengthInteger l _) = l
  lowestBitClear n = lowestBitClear $ toByteString n
  isBitSet n i = isBitSet n (toByteString i)
  lowBitsMask l = VariableLengthInteger l $ lowBitsMask l
  logicalOr a b =
    let v = logicalOr (toByteString a) (toByteString b)
     in VariableLengthInteger (bitLength v) $ toInt v
  logicalAnd a b =
    let v = logicalAnd (toByteString a) (toByteString b)
     in VariableLengthInteger (bitLength v) $ toInt v
  logicalXor a b =
    let v = logicalXor (toByteString a) (toByteString b)
     in VariableLengthInteger (bitLength v) $ toInt v
  shiftRight (VariableLengthInteger l b) i = VariableLengthInteger (l - i `max` 0) $ shiftRight b i
  shiftLeft (VariableLengthInteger l b) i = VariableLengthInteger (l + i) $ shiftLeft b i
  shiftLeftWithBits (VariableLengthInteger la a) l vb@(VariableLengthInteger _ b) =
    if la == 0 then vb else VariableLengthInteger (la + l) $ (a `shiftLeft` l) + b

instance P.ToData VariableLengthInteger where
  toBuiltinData = toBuiltinData . vliInteger

instance P.FromData VariableLengthInteger where
  fromBuiltinData d = fromBuiltinData d >>= maybeFromInt

instance P.UnsafeFromData VariableLengthInteger where
  unsafeFromBuiltinData = fromInt . unsafeFromBuiltinData

instance P.HasBlueprintSchema VariableLengthInteger referencedTypes where
  schema = SchemaInteger emptySchemaInfo emptyIntegerSchema

-- ** Integer

instance ToInt P.Integer where
  toInt = id

instance FromInt P.Integer where
  fromInt = id

instance ToByteString P.Integer where
  toByteString n = integerToByteString BigEndian (bitLengthToByteLength $ bitLength n) n
  byteStringOut = byteStringOut . toByteString

instance FromByteString P.Integer where
  fromByteString = byteStringToInteger BigEndian
  byteStringIn isTerminal = byteStringIn isTerminal <&> toInt @P.BuiltinByteString

instance BitLogic P.Integer where
  bitLength n
    | n < 0 = bitLength (-n - 1) -- two's complement notion of bit length
    | n <= 65535 = bitLength16 n
    | otherwise =
        let findLen l m = if n < m then l else findLen (l + l) (m * m)
            len = findLen 4 4294967296
         in bitLength $ integerToByteString BigEndian len n
  lowestBitClear b = lowestBitSet (-b - 1)
    where
      lowestBitSet n = if n == 0 then -1 else up n 1 2 []
      up n i m ms =
        let (q, r) = n `divMod` m -- m = 2**i, ms list of previous powers of two
         in if r == 0
              then up q (i + i) (m * m) (m : ms)
              else down ms i r (i - 1)
      down [] _ _ h = h -- powers of two, power index, remainder, bits skipped so far
      down (m : ms) i n h =
        let j = i `divide` 2
            (q, r) = n `divMod` m
         in if r == 0
              then down ms j q (h + j)
              else down ms j r h
  isBitSet i n = let e = exponential 2 i in n `remainder` (e + e) >= e
  lowBitsMask l = exponential 2 l - 1
  logicalOr a b = toInt $ logicalOr (toByteString a) (toByteString b)
  logicalAnd a b = toInt $ logicalAnd (toByteString a) (toByteString b)
  logicalXor a b = toInt $ logicalXor (toByteString a) (toByteString b)
  shiftRight b i = b `quotient` exponential 2 i
  shiftLeft b i = b * exponential 2 i
  shiftLeftWithBits a l b = (a `shiftLeft` l) + b

-- ** POSIXTime

instance P.Show POSIXTime where
  showsPrec prec (POSIXTime x) = showApp prec "POSIXTime" [showArg x]

instance ToByteString POSIXTime where
  toByteString = toByteString . getPOSIXTime
  byteStringOut = byteStringOut . getPOSIXTime

instance FromByteString POSIXTime where
  --  fromByteString = POSIXTime . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> POSIXTime

-- ** CurrencySymbol

instance ToByteString CurrencySymbol where
  toByteString = fromByteString . unCurrencySymbol
  byteStringOut = byteStringOut . unCurrencySymbol

instance FromByteString CurrencySymbol where
  --  fromByteString = CurrencySymbol . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> CurrencySymbol

-- PlutusTx only has this for pairs, not longer tuples

-- ** ByteStringCursor

instance P.Eq ByteStringCursor where
  (ByteStringCursor b s e) == (ByteStringCursor b' s' e') = b == b' && s == s' && e == e'

instance P.Show ByteStringCursor where
  showsPrec prec (ByteStringCursor b s e) =
    showApp prec "ByteStringCursor" [showArg b, showArg s, showArg e]

-- ** ByteStringReader

instance P.Functor ByteStringReader where
  fmap = HP.fmap

instance HP.Functor ByteStringReader where
  fmap f (ByteStringReader r) = ByteStringReader $ fmap (first f) . r

instance P.Applicative ByteStringReader where
  pure = HP.pure
  (<*>) = (HP.<*>)

instance HP.Applicative ByteStringReader where
  pure a = ByteStringReader $ \s -> Just (a, s)
  ByteStringReader x <*> ByteStringReader y =
    ByteStringReader $ x >=> (\(f, s') -> y s' <&> first f)

instance Monad ByteStringReader where
  m >>= f =
    ByteStringReader
      ( getByteStringReader m
          >=> (\(a, s') -> getByteStringReader (f a) s')
      )

-- * As

instance (ConvertFrom a b, P.Eq a) => P.Eq (As a b) where
  As x == As y = convertFrom @a @b x == convertFrom @a @b y

instance (ConvertFrom a b, ToByteString a) => ToByteString (As a b) where
  toByteString = toByteString . convertFrom @a @b . getAs
  byteStringOut = byteStringOut . convertFrom @a @b . getAs

instance (ConvertFrom a b, P.ToData a) => P.ToData (As a b) where
  toBuiltinData = toBuiltinData . convertFrom @a @b . getAs

instance (ConvertTo a b, FromByteString a) => FromByteString (As a b) where
  fromByteString = As . convertTo @a @b . fromByteString
  byteStringIn x = As . convertTo @a @b <$> byteStringIn x

instance (ConvertTo a b, P.FromData a) => P.FromData (As a b) where
  fromBuiltinData x = As . convertTo @a @b <$> fromBuiltinData x

instance (ConvertTo a b, P.UnsafeFromData a) => P.UnsafeFromData (As a b) where
  unsafeFromBuiltinData = As . convertTo @a @b . unsafeFromBuiltinData

-- * Helpers

-- ** ByteStrings

isBBSLength :: Integer -> BBS -> Bool
isBBSLength n b = lengthOfByteString b == n

validateFLBBS :: Integer -> BBS -> BBS
validateFLBBS l b = if isBBSLength l b then b else traceError "Wrong size for ByteString"

maybeFLBBSFromInt :: Integer -> Integer -> Maybe BBS
maybeFLBBSFromInt len =
  let isValidInt = isUInt $ 8 * len
   in \n ->
        if isValidInt n
          then
            Just . integerToByteString BigEndian len $ n
          else
            Nothing

-- ** Arithmetics

-- | given len *in bits*, is a given number a non-negative integer of that given length in binary?
isUInt :: Integer -> Integer -> Bool
isUInt len =
  let maxUInt = exponential 2 len - 1
   in \n -> 0 <= n && n <= maxUInt

-- | How is this not in Plutus already?
{-# INLINEABLE multiplyByExponential #-}
multiplyByExponential :: Integer -> Integer -> Integer -> Integer
multiplyByExponential a e n =
  if n == 0
    then a
    else
      let a' = if n `remainder` 2 == 1 then a * e else a
          e' = e * e
          n' = n `quotient` 2
       in multiplyByExponential a' e' n'

-- | How is this not in Plutus already?
exponential :: Integer -> Integer -> Integer
exponential = multiplyByExponential 1

-- *** Strings

builtinByteStringToByteString :: BuiltinByteString -> BS.ByteString
builtinByteStringToByteString (BuiltinByteString b) = b

-- *** Bit-banging utilities

toByte :: Integer -> Byte
toByte = fromInt

toUInt16 :: Integer -> UInt16
toUInt16 = fromInt

toUInt32 :: Integer -> UInt32
toUInt32 = fromInt

toUInt64 :: Integer -> UInt64
toUInt64 = fromInt

toVariableLengthInteger :: Integer -> UInt64
toVariableLengthInteger = fromInt

equalizeByteStringLength :: P.BuiltinByteString -> P.BuiltinByteString -> (P.BuiltinByteString, P.BuiltinByteString)
equalizeByteStringLength a b =
  let la = lengthOfByteString a
      lb = lengthOfByteString b
   in if lb > la
        then (appendByteString (replicateByte (lb - la) 0) a, b)
        else
          if la > lb
            then (a, appendByteString (replicateByte (la - lb) 0) b)
            else (a, b)

bitLengthToByteLength :: Integer -> Integer
bitLengthToByteLength n = (n + 7) `divide` 8

lowBitsMaskFLBBS :: Integer -> Integer -> BBS
lowBitsMaskFLBBS len =
  let allBits = replicateByte len 0xFF
      nBits = 8 * len
   in \l -> shiftByteString allBits $ l - nBits

shiftRightFLBBS :: Integer -> BBS -> Integer -> BBS
shiftRightFLBBS len =
  let noBits = replicateByte len 0
      nBits = 8 * len
   in \fb i ->
        if i < 0
          then
            traceError "Illegal negative index in shiftRight"
          else
            if i > nBits
              then
                noBits
              else
                shiftByteString fb $ -i

{-# INLINEABLE shiftLeftFLBBS #-}
shiftLeftFLBBS :: Integer -> BBS -> Integer -> BBS
shiftLeftFLBBS len =
  let noBits = replicateByte len 0
      nBits = 8 * len
   in \fb i ->
        if i < 0
          then
            traceError ("Illegal negative index in shiftLeft: len=" <> show len <> " i=" <> show i)
          else
            if i > nBits
              then
                noBits
              else
                shiftByteString fb i

-- First argument is the length, second is the start (little endian), last is the bits
extractBitField :: (BitLogic a) => Integer -> Integer -> a -> a
extractBitField len height bits = (bits `shiftRight` height) `logicalAnd` lowBitsMask len

bitLength16 :: Integer -> Integer -- assumes input in [0,65535]
bitLength16 n = if n < 256 then bitLength8 n else 8 + bitLength8 (n `quotient` 256)

bitLength8 :: Integer -> Integer -- assumes input in [0,255]
bitLength8 n = if n < 16 then bitLength4 n else 4 + bitLength4 (n `quotient` 16)

bitLength4 :: Integer -> Integer -- assumes input in [0,15]
bitLength4 n = if n < 4 then bitLength2 n else 2 + bitLength2 (n `quotient` 4)

bitLength2 :: Integer -> Integer -- assumes input in [0,3]
bitLength2 n = min n 2

byteString1, byteString2 :: P.BuiltinByteString
byteString1 = integerToByteString BigEndian 1 0xFF
byteString2 = integerToByteString BigEndian 2 0xFFFF

-- *** ByteString output

{-type ByteStringWriter a = State (BuiltinByteString -> BuiltinByteString) a
writeByteString :: ByteStringOut a => a -> ByteStringWriter ()
writeByteString a = get >>= \ suffix -> put (byteStringOut a . suffix)
byteStringWriterResult :: ByteStringWriter a -> BuiltinByteString
byteStringWriterResult m = execState m emptyByteString-}
toByteStringOut :: (ToByteString a) => a -> P.BuiltinByteString
toByteStringOut a = byteStringOut a Terminal emptyByteString

-- *** ByteString parsing

byteStringInFixedLength :: Integer -> ByteStringReader P.BuiltinByteString
byteStringInFixedLength n =
  ByteStringReader $ \ByteStringCursor {..} ->
    let next = cursorStart + n
     in if next <= cursorEnd
          then
            Just
              ( sliceByteString cursorStart n cursorByteString,
                ByteStringCursor cursorByteString next cursorEnd
              )
          else Nothing

byteStringInToEnd :: ByteStringReader P.BuiltinByteString
byteStringInToEnd =
  ByteStringReader $ \ByteStringCursor {..} ->
    let len = cursorEnd - cursorStart
     in Just
          ( sliceByteString cursorStart len cursorByteString,
            ByteStringCursor cursorByteString cursorEnd cursorEnd
          )

byteStringReaderFail :: ByteStringReader a
byteStringReaderFail = ByteStringReader $ const Nothing

maybeFromByteStringIn_ :: (IsTerminal -> ByteStringReader a) -> P.BuiltinByteString -> Maybe a
maybeFromByteStringIn_ bsIn bs =
  byteStringCursor bs
    & getByteStringReader (bsIn Terminal) >>= \(a, bs') ->
      if emptyByteStringCursor bs' then Just a else Nothing

fromByteStringIn_ :: (IsTerminal -> ByteStringReader a) -> P.BuiltinByteString -> a
fromByteStringIn_ bsIn = fromJust . maybeFromByteStringIn_ bsIn

{-# INLINEABLE fromByteStringIn #-}
fromByteStringIn :: (FromByteString a) => P.BuiltinByteString -> a
fromByteStringIn = fromByteStringIn_ byteStringIn

maybeFromByteStringIn :: (FromByteString a) => P.BuiltinByteString -> Maybe a
maybeFromByteStringIn = maybeFromByteStringIn_ byteStringIn

byteStringCursor :: P.BuiltinByteString -> ByteStringCursor
byteStringCursor bs = ByteStringCursor bs 0 (lengthOfByteString bs)

emptyByteStringCursor :: ByteStringCursor -> Bool
emptyByteStringCursor bsc = cursorStart bsc >= cursorEnd bsc

nextByteStringCursor :: ByteStringCursor -> Maybe (Byte, ByteStringCursor)
nextByteStringCursor bsc =
  if emptyByteStringCursor bsc
    then Nothing
    else
      Just
        ( Byte $ indexByteString (cursorByteString bsc) (cursorStart bsc),
          ByteStringCursor (cursorByteString bsc) (cursorStart bsc + 1) (cursorEnd bsc)
        )

maybeFromByteString :: (FromByteString a) => P.BuiltinByteString -> Maybe a -- XXX DEBUG
maybeFromByteString = maybeFromByteStringIn -- XXX DEBUG
-- fromByteString :: (FromByteString a) => BuiltinByteString -> a -- XXX DEBUG
-- fromByteString = fromJust . maybeFromByteString -- XXX DEBUG

-- ** For Show

showApp :: Integer -> P.BuiltinString -> [P.ShowS] -> P.ShowS
showApp prec funName showArgList =
  showParen (prec > 10) $ showString funName . showSpaced showArgList

showArg :: (P.Show a) => a -> P.ShowS
showArg = showsPrec 11

showSpaced :: [P.ShowS] -> P.ShowS
showSpaced [] = id
showSpaced (sa : sas) = showString " " . sa . showSpaced sas

-- | Data.Maybe.fromJust reimplemented in Plutus-friendly way. Unsafe.
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = traceError "fromJust Nothing"

-- ** For testing and debugging purposes

ofHex :: (FromByteString a) => String -> a
ofHex = fromByteString . toBuiltin . fromJust . decodeHex . pack

hexOf :: (ToByteString a, IsString s) => a -> s
hexOf = fromString . unpack . encodeHex . fromBuiltin . toByteString

bHex :: (ToByteString a) => a -> P.BuiltinString
bHex = hexOf

hexB :: String -> P.BuiltinByteString
hexB = ofHex

-- ** Template Haskell declarations

P.makeLift ''Byte
P.makeIsDataSchemaIndexed ''Byte [('Byte, 0)]

P.makeLift ''UInt16
P.makeIsDataSchemaIndexed ''UInt16 [('UInt16, 0)]

P.makeLift ''UInt32
P.makeIsDataSchemaIndexed ''UInt32 [('UInt32, 0)]

P.makeLift ''UInt64
P.makeIsDataSchemaIndexed ''UInt64 [('UInt64, 0)]

P.makeLift ''UInt256
P.makeIsDataSchemaIndexed ''UInt256 [('UInt256, 0)]

-- P.makeLift ''VariableLengthInteger
-- P.makeIsDataSchemaIndexed ''VariableLengthInteger [('VariableLengthInteger, 0)]

P.makeLift ''IsTerminal
P.makeIsDataSchemaIndexed ''IsTerminal [('NonTerminal, 0), ('Terminal, 1)]

P.makeLift ''Bytes4
P.makeIsDataSchemaIndexed ''Bytes4 [('Bytes4, 0)]

P.makeLift ''Bytes8
P.makeIsDataSchemaIndexed ''Bytes8 [('Bytes8, 0)]

P.makeLift ''Bytes32
P.makeIsDataSchemaIndexed ''Bytes32 [('Bytes32, 0)]

P.makeLift ''Bytes64
P.makeIsDataSchemaIndexed ''Bytes64 [('Bytes64, 0)]
