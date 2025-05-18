{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types where

import Control.Monad (Monad, (>=>))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.String (IsString, String, fromString)
import Data.Text (pack, unpack)
import GHC.Base qualified as GB
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show
import Text.Hex (decodeHex, encodeHex)

-- * Types

-- | Redefining 'Proxy', since Plutus doesn't support `k :: t`
-- e.g. type of type thingy
data Proxy a = Proxy

-- | Backwards composition
(-.) :: (a -> b) -> (b -> c) -> a -> c
(-.) = flip (.)

--- | Type used as a phantom type to indicate a static length of something
data Length = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10 | L16 | L32 | L64

-- | ByteString of statically known length
newtype FixedLengthByteString len
  = FixedLengthByteString {getFixedLengthByteString :: BuiltinByteString}
  deriving anyclass (HasBlueprintDefinition)
  deriving newtype (Show)
  deriving (Eq, ToInt, ToData) via BuiltinByteString

-- | Smart constructor for FixedLengthByteString.
-- Intended to use with TypeApplications, e.g. 'makeFixedLengthByteString @L1 bs'
{-makeFixedLengthByteString :: forall len. (StaticLength len) => BuiltinByteString -> FixedLengthByteString len
makeFixedLengthByteString = FixedLengthByteString-}

-- | Byte
newtype Byte = Byte {getByte :: Integer}
  deriving (Eq, ToInt) via Integer
  deriving newtype (Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | UInt16
newtype UInt16 = UInt16 {getUInt16 :: Integer}
  deriving (Eq, ToInt) via Integer
  deriving stock (Generic)
  deriving newtype (Show)
  deriving anyclass (HasBlueprintDefinition)

-- | FixedLengthInteger
newtype FixedLengthInteger len = FixedLengthInteger {getFixedLengthInteger :: Integer}
  deriving anyclass (HasBlueprintDefinition)

-- | Smart constructor for FixedLengthInteger.
-- Intended to use with TypeApplications, e.g. 'makeFixedLengthInteger @L1 i'
{-makeFixedLengthInteger :: forall len. (StaticLength len) => Integer -> FixedLengthInteger len
makeFixedLengthInteger = FixedLengthInteger-}

-- Make it a newtype VariableLengthInteger = VariableLengthInteger { getVli :: (Integer, Integer) } ???

-- | VariableLengthInteger
data VariableLengthInteger = VariableLengthInteger
  { vliBitLength :: Integer,
    vliInteger :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | Pure Input from ByteString
newtype ByteStringReader a = ByteStringReader
  {getByteStringReader :: ByteStringCursor -> Maybe (a, ByteStringCursor)}

data ByteStringCursor = ByteStringCursor
  { cursorByteString :: BuiltinByteString, -- the bytes
    cursorStart :: Integer, -- start index included
    cursorEnd :: Integer -- end index not included
    --  deriving (Show, Eq)
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- | Wrapper for (r a)
newtype LiftRef r a = LiftRef {liftref :: r a}

-- | Fixed-Point
data Fix f = Fix {getFix :: f (Fix f)}

-- not newtype because Plutus doesn't support recursive newtype
-- Do we want a two-parameter Fix2 f a = Fix2 { getFix2 :: f a (Fix f a) } for functoriality?
-- Or can we "directly" work with some variant of Fix (LiftRef (f a)) ?
-- or some Fix (LiftRef2 f a) where LiftRef2 accomodates for a bifunctor?

-- ** Fixed size data structures

type Bytes4 = FixedLengthByteString 'L4

type Bytes8 = FixedLengthByteString 'L8

type Bytes32 = FixedLengthByteString 'L32

type Bytes64 = FixedLengthByteString 'L64

type UInt32 = FixedLengthInteger 'L4

type UInt64 = FixedLengthInteger 'L8

type UInt256 = FixedLengthInteger 'L32

-- | Wrapper for Isomorphic derivation
-- See https://www.tweag.io/blog/2020-04-23-deriving-isomorphically/
newtype As a b = As { getAs :: b }


-- * Classes

-- | two types are Isomorphic
-- convertTo . convertFrom == id @b
-- convertFrom . convertTo == id @a
-- See https://www.tweag.io/blog/2020-04-23-deriving-isomorphically/
class (ConvertTo a b, ConvertFrom a b) => Isomorphic a b where
class ConvertTo a b where
  convertTo :: a -> b
class ConvertFrom a b where
  convertFrom :: b -> a

-- | Partial types
class Partial a where
  isElement :: a -> Bool

  -- | given an element of the type, return Just it if it is well-formed, Nothing otherwise.
  maybeValidate :: a -> Maybe a
  maybeValidate a = if isElement a then Just a else Nothing

  validate :: a -> a
  validate = fromJust . maybeValidate

-- | Static Length data
class StaticLength (l :: k) where
  staticLength :: Proxy l -> Integer

-- | Only one of toInt and maybeToInt is defined. toInt is unsafe if maybeToInt is partial.
class ToInt a where
  toInt :: a -> Integer
  toInt = fromJust . maybeToInt
  maybeToInt :: a -> Maybe Integer
  maybeToInt = Just . toInt

-- | You need to define one and only one of fromInt or maybeFromInt, the other one will follow.
-- fromInt is not safe unless the function is total, will error out on bad input -- Use with care.
class FromInt a where
  fromInt :: Integer -> a
  fromInt = fromJust . maybeFromInt
  maybeFromInt :: Integer -> Maybe a
  maybeFromInt = Just . fromInt

-- | At least one of toByteString or appendByteStringTerminal must be defined.
-- Additionally, if the output isn't fixed-length or otherwise self-delimited,
-- then byteStringOut must be defined.
-- TODO: replace with something that uses Cardano's standard CBOR encoding.
-- But keep the bytestring variant for the sake of other chains?
data IsTerminal = NonTerminal | Terminal

class ToByteString a where
  -- | convert to String
  toByteString :: a -> BuiltinByteString
  toByteString = toByteStringOut

  -- | append to a String in Terminal or NonTerminal position
  -- default method assumes self-delimitation (e.g. fixed length, length prefix, or terminator)
  byteStringOut :: a -> IsTerminal -> BuiltinByteString -> BuiltinByteString
  byteStringOut a _ = appendByteString $ toByteString a

-- | byteStringIn must always be defined, but
-- fromByteString can be left defaulted, and maybeFromByteString almost always is
-- fromByteString is almost always unsafe, unless every byte string of any length is valid.
class FromByteString a where
  {-# INLINEABLE fromByteString #-}
  fromByteString :: BuiltinByteString -> a
  fromByteString = fromByteStringIn

  --  fromByteString = fromJust . maybeFromByteString
  byteStringIn :: IsTerminal -> ByteStringReader a

--  ??? THIS CAUSES HAVOC IN THE PLUTUS COMPILER, WHY???
--  {-# INLINEABLE maybeFromByteString__ #-}
--  maybeFromByteString__ :: BuiltinByteString -> Maybe a
--  maybeFromByteString__ = maybeFromByteStringIn

{- Failure to include bitLength in either Haskell or Plutus seems incompetent to me. --fare
   (see CIP-123, compare to CLHS integer-length) -}
class BitLogic a where
  bitLength :: a -> Integer
  lowestBitClear :: a -> Integer -- NB: returns nBits for allBits, but -1 for -1 for Integers.
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
  (ToByteString d, FromByteString d, ToData d, FromData d, UnsafeFromData d, Show d, Eq d) =>
  Dato d

class
  (LiftToByteString r, LiftFromByteString r, LiftToData r, LiftFromData r, LiftUnsafeFromData r, LiftShow r, LiftEq r) => -- XXX, LiftHasBlueprintSchema r
  LiftDato r

class LiftEq r where
  liftEq :: (Eq a) => r a -> r a -> Bool

class LiftShow r where
  liftShowsPrec :: (Show a) => Integer -> r a -> ShowS

class LiftToByteString r where
  liftToByteString :: (Dato a) => r a -> BuiltinByteString
  liftToByteString a = liftByteStringOut a Terminal emptyByteString
  liftByteStringOut :: (Dato a) => r a -> IsTerminal -> BuiltinByteString -> BuiltinByteString
  liftByteStringOut a _ = appendByteString $ liftToByteString a

class LiftFromByteString r where
  liftFromByteString :: (FromByteString a) => BuiltinByteString -> r a
  liftFromByteString = fromByteStringIn_ liftByteStringIn
  liftByteStringIn :: (FromByteString a) => IsTerminal -> ByteStringReader (r a)

class LiftToData r where
  liftToBuiltinData :: ToData a => r a -> BuiltinData

class LiftUnsafeFromData r where
  liftUnsafeFromBuiltinData :: UnsafeFromData a => BuiltinData -> r a

class LiftFromData r where
  liftFromBuiltinData :: FromData a => BuiltinData -> Maybe (r a)

class LiftHasBlueprintSchema r where
  liftSchema :: (HasBlueprintSchema a referencedTypes) => (Proxy (r a, a), Schema referencedTypes)

class
  (Monad e, Dato a) =>
  PreWrapping e r a
  where
  wrap :: a -> e (r a)

-- | Wrapping : a value can be wrapped, and wrapped value that can be unwrapped
class
  (PreWrapping e r a) =>
  Wrapping e r a
  where
  unwrap :: r a -> e a

class
  (Monad e) =>
  LiftPreWrapping e r
  where
  liftWrap :: (Dato a) => a -> e (r a)

class
  (LiftPreWrapping e r) =>
  LiftWrapping e r
  where
  liftUnwrap :: (Dato a) => r a -> e a

-- * Instances

-- ** StaticLength

instance StaticLength L0 where
  staticLength = const 0

instance StaticLength L1 where
  staticLength = const 1

instance StaticLength L2 where
  staticLength = const 2

instance StaticLength L3 where
  staticLength = const 3

instance StaticLength L4 where
  staticLength = const 4

instance StaticLength L5 where
  staticLength = const 5

instance StaticLength L6 where
  staticLength = const 6

instance StaticLength L7 where
  staticLength = const 7

instance StaticLength L8 where
  staticLength = const 8

instance StaticLength L9 where
  staticLength = const 9

instance StaticLength L10 where
  staticLength = const 10

instance StaticLength L16 where
  staticLength = const 16

instance StaticLength L32 where
  staticLength = const 32

instance StaticLength L64 where
  staticLength = const 64

-- ** FixedLengthByteString

-- instance
--   (StaticLength len) =>
--   Show (FixedLengthByteString len)
--   where
--   showsPrec prec (FixedLengthByteString b) =
--     showApp prec "FixedLengthByteString" [showArg b]

instance
  (StaticLength len) =>
  Partial (FixedLengthByteString len)
  where
  isElement (FixedLengthByteString b) = lengthOfByteString b == staticLength (Proxy @len)

{-instance
  (StaticLength len) =>
  ToInt (FixedLengthByteString len)
  where
  toInt (FixedLengthByteString b) = toInt b-}

instance
  (StaticLength len) =>
  FromInt (FixedLengthByteString len)
  where
  maybeFromInt =
    let len = staticLength $ Proxy @len
        isValidInt = isUInt $ 8 * len
     in \n ->
          if isValidInt n
            then
              Just . FixedLengthByteString . integerToByteString BigEndian len $ n
            else
              Nothing

instance
  (StaticLength len) =>
  ToByteString (FixedLengthByteString len)
  where
  toByteString (FixedLengthByteString b) = b
  byteStringOut (FixedLengthByteString b) _ = appendByteString b

instance
  (StaticLength len) =>
  FromByteString (FixedLengthByteString len)
  where
  -- {-# INLINEABLE fromByteString #-}
  -- fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  -- maybeFromByteString = maybeValidate . FixedLengthByteString
  byteStringIn _ = byteStringInFixedLength (staticLength $ Proxy @len) <&> FixedLengthByteString

instance
  (StaticLength len) =>
  BitLogic (FixedLengthByteString len)
  where
  bitLength (FixedLengthByteString b) = bitLength b
  lowestBitClear (FixedLengthByteString b) = lowestBitClear b
  isBitSet n (FixedLengthByteString b) = readBit b n
  lowBitsMask =
    let allBits = replicateByte (staticLength $ Proxy @len) 0xFF
        nBits = 8 * staticLength (Proxy @len)
     in \l -> FixedLengthByteString $ shiftByteString allBits $ l - nBits
  logicalOr (FixedLengthByteString a) (FixedLengthByteString b) =
    FixedLengthByteString $ orByteString False a b
  logicalAnd (FixedLengthByteString a) (FixedLengthByteString b) =
    FixedLengthByteString $ andByteString False a b
  logicalXor (FixedLengthByteString a) (FixedLengthByteString b) =
    FixedLengthByteString $ xorByteString False a b
  shiftRight =
    let noBits = replicateByte (staticLength $ Proxy @len) 0
        nBits = 8 * staticLength (Proxy @len)
     in \fb i ->
          if i < 0
            then
              -- traceError "Illegal negative index in shiftRight" -- DEBUG: no trace error for now.
              fb -- DEBUG: NOP while debugging
            else
              FixedLengthByteString
                $ if i > nBits
                  then
                    noBits
                  else
                    shiftByteString (getFixedLengthByteString fb) $ -i
  shiftLeft =
    let noBits = replicateByte (staticLength $ Proxy @len) 0
        nBits = 8 * staticLength (Proxy @len)
     in \fb i ->
          if i < 0
            then
              -- traceError "Illegal negative index in shiftLeft" -- DEBUG: no trace error for now.
              fb -- DEBUG: NOP while debugging
            else
              FixedLengthByteString
                $ if i > nBits
                  then
                    noBits
                  else
                    shiftByteString (getFixedLengthByteString fb) i

instance
  (StaticLength len) =>
  HasBlueprintSchema (FixedLengthByteString len) referencedTypes where
  schema = SchemaBytes emptySchemaInfo emptyBytesSchema

instance
  (StaticLength len) =>
  FromData (FixedLengthByteString len) where
  fromBuiltinData d = fromBuiltinData d <&> fromByteString

instance
  (StaticLength len) =>
  UnsafeFromData (FixedLengthByteString len) where
  unsafeFromBuiltinData = fromByteString . unsafeFromBuiltinData

instance
  (StaticLength len) =>
  Dato (FixedLengthByteString len)

-- ** BuiltinByteString

-- NB: To fit on-chain on Cardano (or affordably on any L1,
-- and thus on any L2 that gets verified by a L2),
-- a BuiltinByteString has to be of length <= 65535 (in practice smaller, more like 8192)
-- For larger data structures... put them in a Trie wherein only a logarithmic fragment is witnessed

instance Partial BuiltinByteString where
  isElement b = lengthOfByteString b <= 65535

instance ToInt BuiltinByteString where
  toInt = fromByteString

instance FromInt BuiltinByteString where
  fromInt = toByteString

instance ToByteString BuiltinByteString where
  toByteString = id
  byteStringOut b Terminal = appendByteString b
  byteStringOut b NonTerminal =
    let len = toUInt16 $ lengthOfByteString b
     in appendByteString (toByteString len) . appendByteString b

instance FromByteString BuiltinByteString where
  fromByteString = id
  byteStringIn Terminal = byteStringInToEnd
  byteStringIn NonTerminal = byteStringIn NonTerminal >>= \(UInt16 len) -> byteStringInFixedLength len

instance BitLogic BuiltinByteString where
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

instance Dato BuiltinByteString

-- ** BuiltinString

instance ToByteString BuiltinString where
  toByteString = encodeUtf8
  byteStringOut = byteStringOut . toByteString

instance FromByteString BuiltinString where
  fromByteString = decodeUtf8 -- XXX
  byteStringIn isTerminal = byteStringIn isTerminal <&> decodeUtf8

instance ToData BuiltinString where
  toBuiltinData = toBuiltinData . toByteString

instance FromData BuiltinString where
  fromBuiltinData d = fromBuiltinData d >>= return . fromByteString

instance UnsafeFromData BuiltinString where
  unsafeFromBuiltinData = fromByteString . unsafeFromBuiltinData

instance Dato BuiltinString


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
  fromByteString =
    Byte
      . byteStringToInteger BigEndian
      . toByteString
      . fromByteString @(FixedLengthByteString L1)

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

instance ToData Byte where
  toBuiltinData = toBuiltinData . getByte

instance FromData Byte where
  fromBuiltinData d = fromBuiltinData d >>= maybeFromInt

instance UnsafeFromData Byte where
  unsafeFromBuiltinData = toByte . unsafeFromBuiltinData

instance HasBlueprintSchema Byte referencedTypes where
  schema = SchemaInteger emptySchemaInfo emptyIntegerSchema

instance Dato Byte

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
  fromByteString =
    UInt16
      . byteStringToInteger BigEndian
      . toByteString @(FixedLengthByteString L2)
      . fromByteString

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

instance ToData UInt16 where
  toBuiltinData = toBuiltinData . getUInt16

instance FromData UInt16 where
  fromBuiltinData d = fromBuiltinData d >>= maybeFromInt

instance UnsafeFromData UInt16 where
  unsafeFromBuiltinData = fromInt . unsafeFromBuiltinData

instance Dato UInt16

instance HasBlueprintSchema UInt16 referencedTypes where
  schema = SchemaInteger emptySchemaInfo emptyIntegerSchema

-- ** FixedLengthInteger

instance
  (StaticLength len) =>
  Eq (FixedLengthInteger len)
  where
  (FixedLengthInteger a) == (FixedLengthInteger b) = a == b

instance
  (StaticLength len) =>
  Show (FixedLengthInteger len)
  where
  showsPrec prec (FixedLengthInteger n) =
    showApp prec "FixedLengthInteger" [showString "@L" . showArg (staticLength $ Proxy @len), showArg n]

instance
  (StaticLength len) =>
  Partial (FixedLengthInteger len)
  where
  isElement = isUInt (8 * staticLength (Proxy @len)) . getFixedLengthInteger

instance ToInt (FixedLengthInteger len) where
  toInt (FixedLengthInteger n) = n

instance
  (StaticLength len) =>
  FromInt (FixedLengthInteger len)
  where
  maybeFromInt = maybeValidate . FixedLengthInteger

instance
  (StaticLength len) =>
  ToByteString (FixedLengthInteger len)
  where
  toByteString (FixedLengthInteger n) = integerToByteString BigEndian (staticLength $ Proxy @len) n

instance
  (StaticLength len) =>
  FromByteString (FixedLengthInteger len)
  where
  --  {-# INLINEABLE fromByteString #-}
  --  fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  --  maybeFromByteString = maybeValidate . FixedLengthInteger . byteStringToInteger BigEndian
  byteStringIn _ =
    byteStringInFixedLength (staticLength $ Proxy @len)
      <&> FixedLengthInteger
      . byteStringToInteger BigEndian

instance
  (StaticLength len) =>
  BitLogic (FixedLengthInteger len)
  where
  bitLength (FixedLengthInteger n) = bitLength $ integerToByteString BigEndian (staticLength $ Proxy @len) n
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
  ToData (FixedLengthInteger len) where
  toBuiltinData = toBuiltinData . getFixedLengthInteger

instance
  (StaticLength len) =>
  FromData (FixedLengthInteger len) where
  fromBuiltinData d = fromBuiltinData d >>= maybeFromInt

instance
  (StaticLength len) =>
  UnsafeFromData (FixedLengthInteger len) where
  unsafeFromBuiltinData = fromInt . unsafeFromBuiltinData

instance
  (StaticLength len) =>
  Dato (FixedLengthInteger len)

instance
  (StaticLength len) =>
  HasBlueprintSchema (FixedLengthInteger len) referencedTypes where
  schema = SchemaInteger emptySchemaInfo emptyIntegerSchema

-- ** VariableLengthInteger

{- Limitation:
 - only integers of length less than 65536
 - I/O only for non-negative integers (for now)
 - we do not reject non-canonical input (leading zeros) -}
instance Eq VariableLengthInteger where
  x == y = vliInteger x == vliInteger y

instance Show VariableLengthInteger where
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

instance ToData VariableLengthInteger where
  toBuiltinData = toBuiltinData . vliInteger

instance FromData VariableLengthInteger where
  fromBuiltinData d = fromBuiltinData d >>= maybeFromInt

instance UnsafeFromData VariableLengthInteger where
  unsafeFromBuiltinData = fromInt . unsafeFromBuiltinData

instance Dato VariableLengthInteger

instance HasBlueprintSchema VariableLengthInteger referencedTypes where
  schema = SchemaInteger emptySchemaInfo emptyIntegerSchema

-- ** Integer

instance ToInt Integer where
  toInt = id

instance FromInt Integer where
  fromInt = id

instance ToByteString Integer where
  toByteString n = integerToByteString BigEndian (bitLengthToByteLength $ bitLength n) n
  byteStringOut = byteStringOut . toByteString

instance FromByteString Integer where
  fromByteString = byteStringToInteger BigEndian
  byteStringIn isTerminal = byteStringIn isTerminal <&> toInt @BuiltinByteString

instance BitLogic Integer where
  bitLength n
    | n < 0 = bitLength (-n - 1) -- two's complement notion of bit length
    | n <= 65535 = bitLength16 n
    | otherwise =
        let findLen l m = if n < m then l else findLen (l + l) (m * m)
            len = findLen 4 4294967296
         in bitLength $ integerToByteString BigEndian len n
  lowestBitClear n = lowestBitSet (-n - 1)
    where
      lowestBitSet b = if b == 0 then -1 else up n 1 2 []
      up b i m ms =
        let (q, r) = b `divMod` m -- m = 2**i, ms list of previous powers of two
         in if r == 0
              then up q (i + i) (m * m) (m : ms)
              else down ms i r (i - 1)
      down [] _ _ h = h -- powers of two, power index, remainder, bits skipped so far
      down (m : ms) i b h =
        let j = i `divide` 2
            (q, r) = b `divMod` m
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

instance Dato Integer

-- ** POSIXTime

instance Show POSIXTime where
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

-- ** Unit

instance ToByteString () where
  toByteString () = emptyByteString
  byteStringOut () _ s = s

instance FromByteString () where
  -- XXX  {-# INLINEABLE fromByteString #-}
  -- XXX  fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  byteStringIn _ = return ()

instance Dato ()

-- ** Sums and Tuples

-- *** Either

instance
  (ToByteString a, ToByteString b) =>
  ToByteString (Either a b)
  where
  byteStringOut (Left a) isTerminal = byteStringOut (Byte 0) NonTerminal . byteStringOut a isTerminal
  byteStringOut (Right b) isTerminal = byteStringOut (Byte 1) NonTerminal . byteStringOut b isTerminal

instance
  (FromByteString a, FromByteString b) =>
  FromByteString (Either a b)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \b ->
      if b == Byte 0
        then byteStringIn isTerminal <&> Left
        else
          if b == Byte 1
            then byteStringIn isTerminal <&> Right
            else byteStringReaderFail

-- *** Maybe

instance
  (ToByteString a) =>
  ToByteString (Maybe a)
  where
  byteStringOut Nothing _ = byteStringOut (Byte 0) NonTerminal
  byteStringOut (Just a) isTerminal = byteStringOut (Byte 1) NonTerminal . byteStringOut a isTerminal

instance
  (FromByteString a) =>
  FromByteString (Maybe a)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \b ->
      if b == Byte 0
        then return Nothing
        else
          if b == Byte 1
            then byteStringIn isTerminal <&> Just
            else byteStringReaderFail

-- *** (,) or builtin Pairs

instance
  (ToByteString a, ToByteString b) =>
  ToByteString (a, b)
  where
  byteStringOut (a, b) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b isTerminal s

instance
  (FromByteString a, FromByteString b) =>
  FromByteString (a, b)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn isTerminal >>= \b ->
        return (a, b)

instance
  (Dato a, Dato b) =>
  Dato (a, b)

-- *** (,,) or builtin Triplets

instance
  (Eq a, Eq b, Eq c) =>
  Eq (a, b, c)
  where
  (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'

instance
  (ToByteString a, ToByteString b, ToByteString c) =>
  ToByteString (a, b, c)
  where
  byteStringOut (a, b, c) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c isTerminal s

instance
  (FromByteString a, FromByteString b, FromByteString c) =>
  FromByteString (a, b, c)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn isTerminal >>= \c ->
          return (a, b, c)

instance
  (Dato a, Dato b, Dato c) =>
  Dato (a, b, c)

-- *** (,,,) or builtin Quadruplets

instance
  (Eq a, Eq b, Eq c, Eq d) =>
  Eq (a, b, c, d)
  where
  (a, b, c, d) == (a', b', c', d') = a == a' && b == b' && c == c' && d == d'

instance
  (ToByteString a, ToByteString b, ToByteString c, ToByteString d) =>
  ToByteString (a, b, c, d)
  where
  byteStringOut (a, b, c, d) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c NonTerminal
      $ byteStringOut d isTerminal s

instance
  (FromByteString a, FromByteString b, FromByteString c, FromByteString d) =>
  FromByteString (a, b, c, d)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn NonTerminal >>= \c ->
          byteStringIn isTerminal >>= \d ->
            return (a, b, c, d)

instance
  (Dato a, Dato b, Dato c, Dato d) =>
  Dato (a, b, c, d)

-- *** (,,,,) or builtin Quintuplets

instance
  (Eq a, Eq b, Eq c, Eq d, Eq e) =>
  Eq (a, b, c, d, e)
  where
  (a, b, c, d, e) == (a', b', c', d', e') =
    a == a' && b == b' && c == c' && d == d' && e == e'

instance
  (ToByteString a, ToByteString b, ToByteString c, ToByteString d, ToByteString e) =>
  ToByteString (a, b, c, d, e)
  where
  byteStringOut (a, b, c, d, e) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c NonTerminal
      $ byteStringOut d NonTerminal
      $ byteStringOut e isTerminal s

instance
  (FromByteString a, FromByteString b, FromByteString c, FromByteString d, FromByteString e) =>
  FromByteString (a, b, c, d, e)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn NonTerminal >>= \c ->
          byteStringIn NonTerminal >>= \d ->
            byteStringIn isTerminal >>= \e ->
              return (a, b, c, d, e)

instance
  (ToData a, ToData b, ToData c, ToData d, ToData e) =>
  ToData (a, b, c, d, e) where
  toBuiltinData (a, b, c, d, e) = toBuiltinData [toBuiltinData a, toBuiltinData b, toBuiltinData c, toBuiltinData d, toBuiltinData e]

instance
  (FromData a, FromData b, FromData c, FromData d, FromData e) =>
  FromData (a, b, c, d, e) where
  fromBuiltinData x = fromBuiltinData x >>= \case
    [aa, bb, cc, dd, ee] -> do
      a <- fromBuiltinData aa
      b <- fromBuiltinData bb
      c <- fromBuiltinData cc
      d <- fromBuiltinData dd
      e <- fromBuiltinData ee
      return (a, b, c, d, e)
    _ -> failNow

instance
  (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c, UnsafeFromData d, UnsafeFromData e) =>
  UnsafeFromData (a, b, c, d, e) where
  unsafeFromBuiltinData x =
    case unsafeFromBuiltinData x of
      [a, b, c, d, e] -> (unsafeFromBuiltinData a, unsafeFromBuiltinData b, unsafeFromBuiltinData c, unsafeFromBuiltinData d, unsafeFromBuiltinData e)
      _ -> failNow

instance
  (Dato a, Dato b, Dato c, Dato d, Dato e) =>
  Dato (a, b, c, d, e)

-- *** (,,,,,) or builtin Sextuplets

instance
  (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
  Eq (a, b, c, d, e, f)
  where
  (a, b, c, d, e, f) == (a', b', c', d', e', f') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

instance
  (ToByteString a, ToByteString b, ToByteString c, ToByteString d, ToByteString e, ToByteString f) =>
  ToByteString (a, b, c, d, e, f)
  where
  byteStringOut (a, b, c, d, e, f) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c NonTerminal
      $ byteStringOut d NonTerminal
      $ byteStringOut e NonTerminal
      $ byteStringOut f isTerminal s

instance
  (FromByteString a, FromByteString b, FromByteString c, FromByteString d, FromByteString e, FromByteString f) =>
  FromByteString (a, b, c, d, e, f)
  where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn NonTerminal >>= \c ->
          byteStringIn NonTerminal >>= \d ->
            byteStringIn NonTerminal >>= \e ->
              byteStringIn isTerminal >>= \f ->
                return (a, b, c, d, e, f)

instance
  (ToData a, ToData b, ToData c, ToData d, ToData e, ToData f) =>
  ToData (a, b, c, d, e, f) where
  toBuiltinData (a, b, c, d, e, f) = toBuiltinData [toBuiltinData a, toBuiltinData b, toBuiltinData c, toBuiltinData d, toBuiltinData e, toBuiltinData f]

instance
  (FromData a, FromData b, FromData c, FromData d, FromData e, FromData f) =>
  FromData (a, b, c, d, e, f) where
  fromBuiltinData x = fromBuiltinData x >>= \case
    [aa, bb, cc, dd, ee, ff] -> do
      a <- fromBuiltinData aa
      b <- fromBuiltinData bb
      c <- fromBuiltinData cc
      d <- fromBuiltinData dd
      e <- fromBuiltinData ee
      f <- fromBuiltinData ff
      return (a, b, c, d, e, f)
    _ -> failNow

instance
  (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c, UnsafeFromData d, UnsafeFromData e, UnsafeFromData f) =>
  UnsafeFromData (a, b, c, d, e, f) where
  unsafeFromBuiltinData x =
    case unsafeFromBuiltinData x of
      [a, b, c, d, e, f] -> (unsafeFromBuiltinData a, unsafeFromBuiltinData b, unsafeFromBuiltinData c, unsafeFromBuiltinData d, unsafeFromBuiltinData e, unsafeFromBuiltinData f)
      _ -> failNow

instance
  (Dato a, Dato b, Dato c, Dato d, Dato e, Dato f) =>
  Dato (a, b, c, d, e, f)


-- ** Lists

instance
  (ToByteString a) =>
  ToByteString [a] -- length limit 65535.
  where
  byteStringOut l isTerminal s =
    byteStringOut (toUInt16 $ length l) NonTerminal $ loop s l
    where
      loop bs [] = bs
      loop bs [a] = byteStringOut a isTerminal bs
      loop bs (a : l') = byteStringOut a NonTerminal (loop bs l')

instance
  (FromByteString a) =>
  FromByteString [a] -- length limit 65535
  where
  byteStringIn isTerminal = byteStringIn NonTerminal >>= \(UInt16 len) -> loop len
    where
      loop n =
        if n == 0
          then return []
          else byteStringIn (if n == 1 then isTerminal else NonTerminal) >>= \a -> loop (n - 1) <&> (a :)

-- ** Identity

instance (Eq a) => Eq (Identity a) where
  x == y = runIdentity x == runIdentity y

instance (Show a) => Show (Identity a) where
  showsPrec prec (Identity x) = showApp prec "Identity" [showArg x]

instance (ToByteString a) => ToByteString (Identity a) where
  toByteString = toByteString . runIdentity
  byteStringOut = byteStringOut . runIdentity

instance (FromByteString a) => FromByteString (Identity a) where
  -- fromByteString = Identity . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> Identity

instance (Dato a) => PreWrapping Identity Identity a where
  wrap = Identity . Identity

instance (Dato a) => Wrapping Identity Identity a where
  unwrap x = x

instance LiftEq Identity where
  liftEq = (==)

instance LiftShow Identity where
  liftShowsPrec = showsPrec

instance LiftToByteString Identity where
  liftByteStringOut = byteStringOut
  liftToByteString = toByteString

instance LiftFromByteString Identity where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn

instance LiftToData Identity where
  liftToBuiltinData = toBuiltinData . runIdentity

instance LiftFromData Identity where
  liftFromBuiltinData = fmap Identity . fromBuiltinData

instance LiftUnsafeFromData Identity where
  liftUnsafeFromBuiltinData = Identity . unsafeFromBuiltinData

--instance LiftHasBlueprintSchema Identity where
--  liftSchema = (Proxy @a, schema @a)

instance LiftDato Identity

instance LiftPreWrapping Identity Identity where
  liftWrap = wrap

instance LiftWrapping Identity Identity where
  liftUnwrap = unwrap

{-
instance LiftSerialise Identity where
  liftEncode = encode . runIdentity
  liftDecode = decode <&> Identity
  liftEncodeList = encodeList . map runIdentity
  liftDecodeList = decodeList <&> map Identity
-}

-- ** Fix: Y-combinator or fixed point combinator for types

instance
  (LiftEq f) =>
  Eq (Fix f)
  where
  (==) x y = liftEq (getFix x) (getFix y)

instance
  (LiftShow f) =>
  Show (Fix f)
  where
  showsPrec prec (Fix x) = showApp prec "Fix" [liftShowsPrec 11 x]

instance
  (LiftDato f) =>
  ToByteString (Fix f)
  where
  toByteString = liftToByteString . getFix
  byteStringOut = liftByteStringOut . getFix

instance
  (LiftToData f) =>
  ToData (Fix f) where
  toBuiltinData = liftToBuiltinData . getFix

instance
  (LiftFromData f) =>
  FromData (Fix f) where
  fromBuiltinData d = liftFromBuiltinData d >>= return . Fix

instance
  (LiftUnsafeFromData f) =>
  UnsafeFromData (Fix f) where
  unsafeFromBuiltinData = liftUnsafeFromBuiltinData -. Fix

instance
  (LiftDato r) =>
  Dato (Fix r)

instance
  (LiftFromByteString f) =>
  FromByteString (Fix f)
  where
  fromByteString = Fix . liftFromByteString
  byteStringIn isTerminal = liftByteStringIn isTerminal <&> Fix

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

-- ** ByteStringCursor

instance Eq ByteStringCursor where
  (ByteStringCursor b s e) == (ByteStringCursor b' s' e') = (b, s, e) == (b', s', e')

instance Show ByteStringCursor where
  showsPrec prec (ByteStringCursor b s e) =
    showApp prec "ByteStringCursor" [showArg b, showArg s, showArg e]

-- ** ByteStringReader

instance GB.Functor ByteStringReader where
  fmap f (ByteStringReader r) = ByteStringReader $ fmap (first f) . r

instance Functor ByteStringReader where
  fmap = GB.fmap

instance Applicative ByteStringReader where
  pure = GB.pure
  (<*>) = (GB.<*>)

instance GB.Applicative ByteStringReader where
  pure a = ByteStringReader $ \s -> Just (a, s)
  ByteStringReader x <*> ByteStringReader y =
    ByteStringReader $ x >=> (\(f, s') -> y s' <&> first f)

instance Monad ByteStringReader where
  m >>= f =
    ByteStringReader
      ( getByteStringReader m
          >=> (\(a, s') -> getByteStringReader (f a) s')
      )

-- ** LiftRef

instance
  (LiftEq r, Eq a) =>
  Eq (LiftRef r a)
  where
  LiftRef x == LiftRef y = x `liftEq` y

instance
  (LiftShow r, Show a) =>
  Show (LiftRef r a)
  where
  showsPrec prec (LiftRef ra) = showApp prec "LiftRef" [liftShowsPrec 11 ra]

instance
  (LiftToData r, ToData a) =>
  ToData (LiftRef r a)
  where
  toBuiltinData = liftToBuiltinData . liftref

instance
  (LiftUnsafeFromData r, UnsafeFromData a) =>
  UnsafeFromData (LiftRef r a)
  where
  unsafeFromBuiltinData = LiftRef . liftUnsafeFromBuiltinData

instance
  (LiftFromData r, FromData a) =>
  FromData (LiftRef r a)
  where
  fromBuiltinData d = liftFromBuiltinData d >>= return . LiftRef

instance
  (LiftDato r, Dato a) =>
  Dato (LiftRef r a)

instance
  (Functor r) =>
  Functor (LiftRef r)
  where
  fmap f (LiftRef x) = LiftRef (fmap f x)

instance
  (LiftToByteString r, Dato a) =>
  ToByteString (LiftRef r a)
  where
  toByteString = liftToByteString . liftref
  byteStringOut = liftByteStringOut . liftref

instance
  (LiftFromByteString r, FromByteString a) =>
  FromByteString (LiftRef r a)
  where
  byteStringIn isTerminal = liftByteStringIn isTerminal <&> LiftRef

instance
  (LiftPreWrapping e r, Dato a) =>
  PreWrapping e (LiftRef r) a
  where
  wrap a = do ra <- liftWrap a; return $ LiftRef ra

instance
  (LiftWrapping e r, Dato a) =>
  Wrapping e (LiftRef r) a
  where
  unwrap = liftUnwrap . liftref

-- * As
instance (ConvertFrom a b, Eq a) => Eq (As a b) where
  As x == As y = convertFrom @a @b x == convertFrom @a @b y

instance (ConvertFrom a b, ToByteString a) => ToByteString (As a b) where
  toByteString = toByteString . convertFrom @a @b . getAs
  byteStringOut = byteStringOut . convertFrom @a @b . getAs
instance (ConvertFrom a b, ToData a) => ToData (As a b) where
  toBuiltinData = toBuiltinData . convertFrom @a @b . getAs

instance (ConvertTo a b, FromByteString a) => FromByteString (As a b) where
  fromByteString = As . convertTo @a @b . fromByteString
  byteStringIn x = byteStringIn x >>= return . As . convertTo @a @b
instance (ConvertTo a b, FromData a) => FromData (As a b) where
  fromBuiltinData x = fromBuiltinData x >>= return . As . convertTo @a @b
instance (ConvertTo a b, UnsafeFromData a) => UnsafeFromData (As a b) where
  unsafeFromBuiltinData = As . convertTo @a @b . unsafeFromBuiltinData

-- * Helpers

-- ** Arithmetics

-- | given len *in bits*, is a given number a non-negative integer of that given length in binary?
isUInt :: Integer -> Integer -> Bool
isUInt len =
  let maxUInt = exponential 2 len - 1
   in \n -> 0 <= n && n <= maxUInt

-- | How is this not in Plutus already?
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

equalizeByteStringLength :: BuiltinByteString -> BuiltinByteString -> (BuiltinByteString, BuiltinByteString)
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

byteString1, byteString2 :: BuiltinByteString
byteString1 = integerToByteString BigEndian 1 0xFF
byteString2 = integerToByteString BigEndian 2 0xFFFF

-- *** ByteString output

{-type ByteStringWriter a = State (BuiltinByteString -> BuiltinByteString) a
writeByteString :: ByteStringOut a => a -> ByteStringWriter ()
writeByteString a = get >>= \ suffix -> put (byteStringOut a . suffix)
byteStringWriterResult :: ByteStringWriter a -> BuiltinByteString
byteStringWriterResult m = execState m emptyByteString-}
toByteStringOut :: (ToByteString a) => a -> BuiltinByteString
toByteStringOut a = byteStringOut a Terminal emptyByteString

-- *** ByteString parsing

byteStringInFixedLength :: Integer -> ByteStringReader BuiltinByteString
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

byteStringInToEnd :: ByteStringReader BuiltinByteString
byteStringInToEnd =
  ByteStringReader $ \ByteStringCursor {..} ->
    let len = cursorEnd - cursorStart
     in Just
          ( sliceByteString cursorStart len cursorByteString,
            ByteStringCursor cursorByteString cursorEnd cursorEnd
          )

byteStringReaderFail :: ByteStringReader a
byteStringReaderFail = ByteStringReader $ const Nothing

maybeFromByteStringIn_ :: (IsTerminal -> ByteStringReader a) -> BuiltinByteString -> Maybe a
maybeFromByteStringIn_ bsIn bs =
  byteStringCursor bs
    & getByteStringReader (bsIn Terminal) >>= \(a, bs') ->
      if emptyByteStringCursor bs' then Just a else Nothing

fromByteStringIn_ :: (IsTerminal -> ByteStringReader a) -> BuiltinByteString -> a
fromByteStringIn_ bsIn = fromJust . maybeFromByteStringIn_ bsIn

{-# INLINEABLE fromByteStringIn #-}
fromByteStringIn :: (FromByteString a) => BuiltinByteString -> a
fromByteStringIn = fromByteStringIn_ byteStringIn

maybeFromByteStringIn :: (FromByteString a) => BuiltinByteString -> Maybe a
maybeFromByteStringIn = maybeFromByteStringIn_ byteStringIn

byteStringCursor :: BuiltinByteString -> ByteStringCursor
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

maybeFromByteString :: (FromByteString a) => BuiltinByteString -> Maybe a -- XXX DEBUG
maybeFromByteString = maybeFromByteStringIn -- XXX DEBUG
-- fromByteString :: (FromByteString a) => BuiltinByteString -> a -- XXX DEBUG
-- fromByteString = fromJust . maybeFromByteString -- XXX DEBUG

-- *** Sums and Tuples

-- | Data.Maybe.fromJust reimplemented in Plutus-friendly way. Unsafe.
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = traceError "fromJust Nothing"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 g (a, b, c, d, e, f) = g a b c d e f

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a, b, c, d)

curry5 :: ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f
curry5 f a b c d e = f (a, b, c, d, e)

curry6 :: ((a, b, c, d, e, f) -> g) -> a -> b -> c -> d -> e -> f -> g
curry6 g a b c d e f = g (a, b, c, d, e, f)

-- ** For Show

showApp :: Integer -> BuiltinString -> [ShowS] -> ShowS
showApp prec funName showArgList =
  showParen (prec > 10) $ showString funName . showSpaced showArgList

showArg :: (Show a) => a -> ShowS
showArg = showsPrec 11

showSpaced :: [ShowS] -> ShowS
showSpaced [] = id
showSpaced (sa : sas) = showString " " . sa . showSpaced sas

-- ** For testing and debugging purposes

ofHex :: (FromByteString a) => String -> a
ofHex = fromByteString . toBuiltin . fromJust . decodeHex . pack

hexOf :: (ToByteString a, IsString s) => a -> s
hexOf = fromString . unpack . encodeHex . fromBuiltin . toByteString

bHex :: (ToByteString a) => a -> BuiltinString
bHex = hexOf

hexB :: String -> BuiltinByteString
hexB = ofHex

failNow :: a
failNow = traceError "FOO"

-- Plutus refuses to compile noReturn = noReturn
-- which isn't because it won't loop forever at times when not asked!
noReturn :: a -> a
noReturn = noReturn

trace' :: (Show s) => s -> e -> e
trace' s = trace (show s)

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

-- * Meta declarations
