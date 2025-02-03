{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Utils (PreWrapping, Wrapping, Lift (..), LiftBinary, LiftShow, LiftEq, Digestible, DigestRef (..), DigestOnly (..), Blake2b_256_Ref, HashAlgorithmOf, wrap, unwrap, integerLength, lowestBitSet, lowestBitClear, fbLowestBitSet, fbLowestBitClear, lowBitsMask, extractBitField, lookupDigest, liftEq, liftShow, liftGet, liftPut, getDigest, digestiblePut, computeDigest) where

import Crypto.Hash
import Data.Binary (Binary, Get, Put, encode, get, put) -- , decode
-- import Data.Binary.Get.Internal (readN)
-- import Data.Function ((&))
import Data.Bits (Bits, FiniteBits, complement, countTrailingZeros, shiftL, shiftR, (.&.))
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Identity (Identity (..))
import Data.Internal.RecursionSchemes
import Data.Kind (Type)
import Data.Maybe (fromJust)

-- Wrappers (reference)
-- Functor r =>
class
  (Monad e) =>
  PreWrapping a r e
  where
  wrap :: a -> e (r a)

class
  (PreWrapping a r e) =>
  Wrapping a r e
  where
  unwrap :: r a -> e a

class LiftBinary r where
  liftGet :: (Binary a) => Get (r a)
  liftPut :: (Binary a) => r a -> Put

class LiftEq r where
  liftEq :: (Eq a) => (r a) -> (r a) -> Bool

class LiftShow r where
  liftShow :: (Show a) => (r a) -> String

data Lift r a = Lift {lifted :: r a}
  deriving (Functor)

instance
  (LiftEq r, Eq a) =>
  Eq (Lift r a)
  where
  (==) x y = liftEq (lifted x) (lifted y)

instance
  (LiftShow r, Show a) =>
  Show (Lift r a)
  where
  show = liftShow . lifted

instance
  (LiftBinary r, Binary a) =>
  Binary (Lift r a)
  where
  get = (liftGet :: Get (r a)) >>= (pure . Lift)
  put = liftPut . lifted

instance
  (LiftShow f) =>
  Show (Fix f)
  where
  show = liftShow . out

instance
  (LiftEq f) =>
  Eq (Fix f)
  where
  (==) x y = liftEq (out x) (out y)

instance LiftShow Identity where
  liftShow = show . runIdentity

instance LiftEq Identity where
  liftEq x y = (runIdentity x) == (runIdentity y)

instance LiftBinary Identity where
  liftGet = get >>= pure . Identity
  liftPut = put . runIdentity

-- instance Functor r => Wrapping r Identity where
--  wrap = pure

instance PreWrapping a Identity Identity where
  wrap = Identity . Identity

instance Wrapping a Identity Identity where
  unwrap x = x

instance PreWrapping a (Lift Identity) Identity where
  wrap = Identity . Lift . Identity

instance Wrapping a (Lift Identity) Identity where
  unwrap = lifted

computeDigest :: (Binary a, HashAlgorithm ha) => a -> Digest ha
computeDigest = hashlazy . encode

class
  (Binary (Digest (HashAlgorithmOf r)), HashAlgorithm (HashAlgorithmOf r)) =>
  Digestible r
  where
  type HashAlgorithmOf r :: Type
  getDigest :: r a -> Digest (HashAlgorithmOf r)

data DigestRef ha x = DigestRef {digestRefValue :: x, digestRefDigest :: Digest ha}

instance Eq (DigestRef ha x) where
  (DigestRef _ ah) == (DigestRef _ bh) = ah == bh

data DigestOnly ha x = DigestOnly {digestOnly :: Digest ha}
  deriving (Eq, Show)

instance Digestible Blake2b_256_Ref where
  type HashAlgorithmOf Blake2b_256_Ref = Blake2b_256
  getDigest = digestRefDigest

instance Digestible Blake2b_256_Only where
  type HashAlgorithmOf Blake2b_256_Only = Blake2b_256
  getDigest = digestOnly

type Blake2b_256_Ref = DigestRef Blake2b_256

type Blake2b_256_Only = DigestOnly Blake2b_256

instance (Binary a, HashAlgorithm ha) => PreWrapping a (DigestRef ha) Identity where
  wrap x = Identity (DigestRef x $ computeDigest x)

instance (Binary a, HashAlgorithm ha) => Wrapping a (DigestRef ha) Identity where
  unwrap (DigestRef x _) = Identity x

lookupDigest :: (HashAlgorithm h) => Digest h -> a
lookupDigest = error "Cannot get a value from its digest"

instance Binary (Digest Blake2b_256) where
  put = put . BS.pack . BA.unpack
  get = (fromJust . digestFromByteString) `fmap` (get :: Get BSS.ByteString)

instance LiftBinary Blake2b_256_Ref where
  liftPut = put . getDigest
  liftGet = error "NIY"

digestiblePut :: (Digestible r, Binary a) => r a -> Put
digestiblePut = put . getDigest

-- digestibleGet :: (Digestible r, Binary a) => Get (r a)
-- digestibleGet = error "NIY" -- lookupDigest . (get :: Get (Digest (HashAlgorithmOf r)))

-- instance (Binary (Digest ha), HashAlgorithm ha) => LiftBinary (DigestRef ha) where
--   liftPut = put . getDigest
--   liftGet = lookupDigest . (get :: Get (Digest (HashAlgorithmOf r)))

-- instance Digestible r => LiftBinary r where
--   liftPut = put . getDigest
--   liftGet = lookupDigest . (get :: Get (Digest (HashAlgorithmOf r)))

-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
integerLength :: (Bits n, Integral n, Integral l) => n -> l
integerLength n =
  if n > 0
    then 1 + (integerLength (n `shiftR` 1))
    else
      if n == 0
        then 0
        else integerLength (-n-1)

-- TODO: a variant that returns both bit and height
-- TODO: make it work efficiently on Bits as well as FiniteBits???
lowestBitSet :: (Bits n, Integral n) => n -> Int
lowestBitSet n = if n .&. 1 > 0 then 0 else if n == 0 then -1 else 1 + lowestBitSet (n `shiftR` 1)

fbLowestBitSet :: (FiniteBits n, Integral n) => n -> Int
fbLowestBitSet n = if n == 0 then -1 else countTrailingZeros n

-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
lowestBitClear :: (Bits n, Integral n) => n -> Int
lowestBitClear n =
  if n .&. 1 == 0 then 0 else if n == -1 then -1 else 1 + lowestBitClear (n `shiftR` 1)

fbLowestBitClear :: (FiniteBits n, Integral n) => n -> Int
fbLowestBitClear n = if n == -1 then -1 else countTrailingZeros $ complement n

lowBitsMask :: (Bits n, Integral n) => Int -> n
lowBitsMask i = (1 `shiftL` i) - 1

-- TODO: more efficient implementation?
-- First argument is the length, second is the start (little endian), last is the bits
extractBitField :: (Bits n, Integral n) => Int -> Int -> n -> n
extractBitField len start bits =
  (bits `shiftR` start) .&. (lowBitsMask len)
