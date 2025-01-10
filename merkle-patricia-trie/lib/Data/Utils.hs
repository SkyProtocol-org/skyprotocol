{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

-- TODO use Data.Binary for I/O

module Data.Utils (PreWrapping, Wrapping, wrap, unwrap, integerLength, lowestBitSet, lowestBitClear, lowBitsMask, extractBitField) where

import Data.Bits (Bits, FiniteBits, (.&.), shiftR, shiftL, countTrailingZeros)
import Data.Binary () -- Get, Put
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)

-- Wrappers (reference)
-- Functor r =>
class Monad e => PreWrapping r e where
  wrap :: a -> e (r a)

class PreWrapping r e => Wrapping r e where
  unwrap :: r a -> e a

-- instance Functor r => Wrapping r Identity where
--  wrap = pure

instance PreWrapping Identity Identity where
  wrap = Identity . Identity
instance Wrapping Identity Identity where
  unwrap x = x

-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
integerLength :: (Bits n, Integral n, Integral l) => n -> l
integerLength n =
  if n > 0 then 1 + (integerLength (n `shiftR` 1)) else
  if n == 0 then 0 else
  integerLength (- n)

-- TODO: a variant that returns both bit and height
-- TODO: make it work on Bits as well as FiniteBits???
lowestBitSet :: (FiniteBits n, Integral n) => n -> Int
lowestBitSet n = if n == 0 then -1 else 1 + countTrailingZeros n
-- Works (badly) on Bits rather than FiniteBits: if n .&. 1 > 0 then 0 else if n == 0 then -1 else 1 + lowestBitSet (n `shiftR` 1)

-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
lowestBitClear :: (FiniteBits n, Integral n, Integral l) => n -> l
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
