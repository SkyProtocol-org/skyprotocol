{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Trie.Internal where

import Data.Bits
import Data.Kind (Type)
import Prelude hiding (lookup)

data Trie' k h a
  = Empty
  | Leaf {key :: k, value :: a}
  | -- | Branch node stores branching bit `bBit` and longest common prefix `pref`
    Branch {height :: h, prefix :: k, left :: Trie' k h a, right :: Trie' k h a}
  deriving (Show, Eq, Functor)

-- | Constraint for Trie key and height. Includes default implementation for little endian Tries
-- TODO replace default with big endian Trie
class (Bits k, Num k, Integral k, Integral (TrieHeight k)) => TrieKey k where
  -- | This will allow us to enforce the height of the trie depending on it's key
  type TrieHeight k :: Type

  -- | Discards bits not in the mask
  mask :: k -> k -> k
  mask k m = k .&. (m - 1)

  -- | Finds the first bit on which prefixes disagree.
  commonBranchingBit :: k -> k -> k
  commonBranchingBit p1 p2 = lowestBit (p1 `xor` p2)
    where
      lowestBit :: k -> k
      lowestBit x = x .&. complement x

  -- | Masks key using supplied branching bit and compares to prefix
  matchPrefix :: k -> k -> k -> Bool
  matchPrefix key prefix m = mask key m == prefix

  -- | Converts height 'TrieHeight k' into the branching bit 'k'
  heightToBBit :: TrieHeight k -> k
  heightToBBit h = 2 ^ h

  -- | Converts branching bit 'k' into height 'TrieHeight k'
  bBitToHeight :: k -> TrieHeight k
  bBitToHeight = floor . logBase 2.0 . fromIntegral

  -- | Tests whether the desired bit is zero
  zeroBit :: k -> k -> Bool
  zeroBit k m = (k .&. m) == 0

type Trie k a = Trie' k (TrieHeight k) a

branch :: (TrieKey k) => k -> k -> Trie k a -> Trie k a -> Trie k a
branch bBit = Branch (bBitToHeight bBit)

join :: (TrieKey k) => k -> Trie k a -> k -> Trie k a -> Trie k a
join p1 t1 p2 t2 =
  if zeroBit p1 bBit
    then branch bBit (mask p1 bBit) t1 t2
    else branch bBit (mask p1 bBit) t2 t1
  where
    bBit = commonBranchingBit p1 p2
