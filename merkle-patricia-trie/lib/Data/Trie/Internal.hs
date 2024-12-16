{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trie.Internal where

import Data.Bits
import Data.Kind (Type)
import Data.WideWord (Word256)
import Data.Word (Word8)
import Prelude hiding (lookup)

data Trie' (w :: Type -> Type) k h a
  = Empty
  | Leaf k (w a)
  | Branch k k (Trie' w k h a) (Trie' w k h a)
  deriving (Functor)

-- | This will allow us to enforce the height of the trie depending on it's key
type family TrieHeight key where
  TrieHeight (LEK Word256) = Word8
  TrieHeight (BEK Word256) = Word8

-- | This will ensure, that when someone tries to create a Trie, the height will be automagically supplied
type Trie w k a = Trie' w k (TrieHeight k) a

type LittleTrie w k a = Trie w (LEK k) a

type BigTrie w k a = Trie w (BEK k) a

class (Bits k, Num k, Integral k) => TrieKey k where
  -- | Discards bits before/after(depending on endianess) branching bit
  mask :: k -> k -> k

  -- | Leaves only the branching bit(so key 0b1001101 becomes 0b0001000, if 4th bit was a branching bit)
  branchingBit :: k -> k -> k

  matchPrefix :: k -> k -> k -> Bool
  matchPrefix bBit prefix key = mask bBit key == prefix

  zeroBit :: k -> k -> Bool
  zeroBit a b = (a .&. b) == 0

newtype LEK a = LittleEndian a deriving (Enum, Eq, Ord, Real, Bits, Num, Integral)

newtype BEK a = BigEndian a deriving (Enum, Eq, Ord, Real, Bits, Num, Integral)

instance (Bits k, Num k, Integral k) => TrieKey (LEK k) where
  mask (LittleEndian a) (LittleEndian b) = LittleEndian $ a .&. (b - 1)
  branchingBit (LittleEndian a) (LittleEndian b) = LittleEndian . lowestBit $ a `xor` b
    where
      lowestBit x = x .&. complement x

instance (Bits k, Num k, Integral k) => TrieKey (BEK k) where
  mask (BigEndian a) (BigEndian b) = BigEndian $ (a .|. (b - 1)) .&. complement b

  -- TODO figure out how to properly implement branchingBit for BigEndian
  -- since it needs more args than just a and b(it also needs prefixes)
  branchingBit _ _ = undefined

join :: (TrieKey k) => k -> Trie w k a -> k -> Trie w k a -> Trie w k a
join p1 t1 p2 t2 =
  if zeroBit p1 bBit
    then Branch bBit (mask p1 bBit) t1 t2
    else Branch bBit (mask p1 bBit) t2 t1
  where
    bBit = branchingBit p1 p2
