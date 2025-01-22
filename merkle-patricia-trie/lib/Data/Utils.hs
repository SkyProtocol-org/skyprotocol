{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE GADTs #-}
-- TODO use Data.Binary for I/O

module Data.Utils (PreWrapping, Wrapping, DigestWrap (..), wrap, unwrap, integerLength, lowestBitSet, lowestBitClear, lowBitsMask, extractBitField, lookupDigest) where

import Crypto.Hash
import Data.Bits (Bits, (.&.), shiftR, shiftL) -- FiniteBits, countTrailingZeros
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BS
import Data.Binary (Binary, Get, encode, put, get) -- Put, decode
-- import Data.Binary.Get.Internal (readN)
-- import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
-- import Data.Kind (Type)

-- Wrappers (reference)
-- Functor r =>
class Monad e => PreWrapping a r e where
  wrap :: a -> e (r a)

class PreWrapping a r e => Wrapping a r e where
  unwrap :: r a -> e a

-- instance Functor r => Wrapping r Identity where
--  wrap = pure

instance PreWrapping a Identity Identity where
  wrap = Identity . Identity
instance Wrapping a Identity Identity where
  unwrap x = x

digest :: (Binary a, HashAlgorithm h) => a -> Digest h
digest = hashlazy . encode

data DigestWrap h x = DigestWrap { wrappedValue :: x, wrappedDigest :: Digest h }

instance (Binary a, HashAlgorithm h) => PreWrapping a (DigestWrap h) Identity where
  wrap x = Identity (DigestWrap x $ digest x)

instance (Binary a, HashAlgorithm h) => Wrapping a (DigestWrap h) Identity where
  unwrap (DigestWrap x _) = Identity x

lookupDigest :: HashAlgorithm h => Digest h -> a
lookupDigest = error "Cannot get a value from its digest"

instance Binary (Digest Blake2b_256) where
  put = put . BS.pack . BA.unpack
  get = error "Not Implemented Yet" -- readN 32 ?

instance (Binary a, HashAlgorithm h, Binary (Digest h)) => Binary (DigestWrap h a) where
  put (DigestWrap _ d) = put d
  get = lookupDigest <$> (get :: Get (Digest h))

-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
integerLength :: (Bits n, Integral n, Integral l) => n -> l
integerLength n =
  if n > 0 then 1 + (integerLength (n `shiftR` 1)) else
  if n == 0 then 0 else
  integerLength (- n)

-- TODO: a variant that returns both bit and height
-- TODO: make it work efficiently on Bits as well as FiniteBits???
lowestBitSet :: (Bits n, Integral n) => n -> Int
lowestBitSet n = if n .&. 1 > 0 then 0 else if n == 0 then -1 else 1 + lowestBitSet (n `shiftR` 1)

-- lowestBitSet :: (FiniteBits n, Integral n) => n -> Int
-- lowestBitSet n = if n == 0 then -1 else 1 + countTrailingZeros n


-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
lowestBitClear :: (Bits n, Integral n, Integral l) => n -> l
lowestBitClear n =
  if n .&. 1 == 0 then 0 else
  if n == -1 then -1 else
  fromInteger (1 + lowestBitClear (n `shiftR` 1))

lowBitsMask :: (Bits n, Integral n) => Int -> n
lowBitsMask i = (1 `shiftL` i) - 1

-- TODO: more efficient implementation?
extractBitField :: (Bits n, Integral n) => Int -> Int -> n -> n
extractBitField len start bits =
  (bits `shiftR` start) .&. (lowBitsMask len)

{- How the hell do I express something like that???
class MerkleRef r where
  get : Binary a  => Binary (r a)
-}
