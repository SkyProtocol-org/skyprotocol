{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trie.Internal where

import Data.Bits
import Data.Kind (Type)
import Prelude hiding (lookup)

data Trie (f :: Type -> Type) k a
  = Empty
  | Leaf k (f a)
  | Branch k k (Trie f k a) (Trie f k a)

class (Bits k, Num k, Integral k) => TrieKey k where
  -- | Discards bits before/after(depending on endianess) branching bit
  mask :: k -> k -> k

  -- | Leaves only the branching bit(so key 0b1001101 becomes 0b0001000, if 4th bit was branching)
  branchingBit :: k -> k -> k

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

matchPrefix :: (TrieKey k) => k -> k -> k -> Bool
matchPrefix k prefix key = mask k key == prefix

zeroBit :: (TrieKey k) => k -> k -> Bool
zeroBit a b = (a .&. b) == 0

join :: (TrieKey k) => k -> Trie f k a -> k -> Trie f k a -> Trie f k a
join p1 t1 p2 t2 =
  if zeroBit p1 commonPrefix
    then Branch (mask p1 commonPrefix) commonPrefix t1 t2
    else Branch (mask p1 commonPrefix) commonPrefix t2 t1
  where
    commonPrefix = branchingBit p1 p2
