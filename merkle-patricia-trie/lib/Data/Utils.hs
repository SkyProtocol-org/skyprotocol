{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Utils (PreWrapping, Wrapping, Lift (..), LiftBinary, LiftShow, LiftEq, Digestible, DigestRef (..), DigestOnly (..), Blake2b_256_Ref, HashAlgorithmOf, wrap, unwrap, integerLength, fbIntegerLength, fbuIntegerLength, lowestBitSet, lowestBitClear, fbLowestBitSet, fbLowestBitClear, fbuLowestBitClear, lowBitsMask, extractBitField, lookupDigest, liftEq, liftShow, liftGet, liftPut, getDigest, digestiblePut, computeDigest, trace', trace1, trace2, trace3, etrace1, etrace2, etrace3) where

import Debug.Trace
import Crypto.Hash
import Control.Monad
import Data.Binary (Binary, Get, Put, encode, get, put) -- , decode
-- import Data.Binary.Get.Internal (readN)
-- import Data.Function ((&))
import Data.Bits (Bits, FiniteBits, complement, finiteBitSize, countLeadingZeros, countTrailingZeros, shiftL, shiftR, (.&.))
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

instance
  (LiftBinary f) =>
  Binary (Fix f)
  where
  put = liftPut . out
  get = (liftGet :: Get (f (Fix f))) >>= (pure . In)


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
  (Binary (Digest (HashAlgorithmOf r)), HashAlgorithm (HashAlgorithmOf r), Show (Digest (HashAlgorithmOf r))) =>
  Digestible r
  where
  type HashAlgorithmOf r :: Type
  getDigest :: r a -> Digest (HashAlgorithmOf r)

data DigestRef ha x = DigestRef {digestRefValue :: x, digestRefDigest :: Digest ha}

instance LiftShow (DigestRef ha) where
  liftShow = show . digestRefDigest

instance Eq (DigestRef ha x) where
  (DigestRef _ ah) == (DigestRef _ bh) = ah == bh

instance (HashAlgorithm ha, Binary (Digest ha)) => Binary (DigestRef ha x) where
   put = put . digestRefDigest
   get = fmap lookupDigest (get :: Get (Digest ha))

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

instance (Binary a, HashAlgorithm ha) => PreWrapping a (Lift (DigestRef ha)) Identity where
  wrap x = Identity (Lift $ DigestRef x $ computeDigest x)

instance (Binary a, HashAlgorithm ha) => Wrapping a (Lift (DigestRef ha)) Identity where
  unwrap (Lift (DigestRef x _)) = Identity x

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

fbIntegerLength :: (FiniteBits n, Integral n, Integral l) => n -> l
fbIntegerLength n =
  if n >= 0 then fbuIntegerLength n else fbuIntegerLength $ complement n

-- assumes non-negative
fbuIntegerLength :: (FiniteBits n, Integral n, Integral l) => n -> l
fbuIntegerLength n = fromIntegral $ finiteBitSize n - countLeadingZeros n

-- TODO: a variant that returns both bit and height
-- TODO: make it work efficiently on Bits as well as FiniteBits???
lowestBitSet :: (Bits n, Integral n, Show n) => n -> Int
lowestBitSet n =
-- trace (show ("lowestBitSet",n)) $
  if n .&. 1 > 0 then 0 else if n == 0 then -1 else 1 + lowestBitSet (n `shiftR` 1)

fbLowestBitSet :: (FiniteBits n, Integral n, Show n) => n -> Int
fbLowestBitSet n =
-- trace (show ("fbLowestBitSet",n)) $
  if n == 0 then -1 else countTrailingZeros n

-- TODO: use a standard library function for that, or at least optimize to logarithmically faster
-- TODO: a variant that returns both bit and height
lowestBitClear :: (Bits n, Integral n, Show n) => n -> Int
lowestBitClear n =
-- trace (show ("lowestBitClear",n)) $
  if n .&. 1 == 0 then 0 else if n == -1 then -1 else 1 + lowestBitClear (n `shiftR` 1)

fbLowestBitClear :: (FiniteBits n, Integral n, Show n) => n -> Int
fbLowestBitClear n =
-- trace (show ("fbLowestBitClear",n)) $
  if n == -1 then -1 else countTrailingZeros $ complement n

-- assumes unsigned
fbuLowestBitClear :: (FiniteBits n, Integral n, Show n) => n -> Int
fbuLowestBitClear n =
-- trace (show ("fbuLowestBitClear",n)) $
  countTrailingZeros $ complement n

lowBitsMask :: (Bits n, Integral n) => Int -> n
lowBitsMask i = if i == -1 then -1 else (1 `shiftL` i) - 1

-- TODO: more efficient implementation?
-- First argument is the length, second is the start (little endian), last is the bits
extractBitField :: (Bits n, Integral n) => Int -> Int -> n -> n
extractBitField len start bits =
  (bits `shiftR` start) .&. (lowBitsMask len)

trace':: (Show s) => s -> e -> e
trace' s e = trace (show s) e

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
