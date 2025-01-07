{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Internal.Trie where

import Data.Bits
import Data.Internal.RecursionSchemes
import Data.Kind (Type)
import Prelude hiding (lookup)

data TrieF k h v a
  = Empty
  | Leaf {key :: k, value :: v}
  | -- | Branch node stores branching bit `bBit` and longest common prefix `pref`
    Branch {height :: h, prefix :: k, left :: a, right :: a}
  deriving (Show, Eq, Functor)

-- | Constraint for Trie key and height. Provides default methods for calculating neccessary info.
class (FiniteBits k, Bits k, Num k, Integral k, Integral (TrieHeight k), Bits (TrieHeight k), Num (TrieHeight k)) => TrieKey k where
  -- | This will allow us to enforce the height of the trie depending on it's key
  type TrieHeight k :: Type

  -- | Discards bits not in the mask
  mask :: k -> k -> k
  mask k m = k .&. (m - 1)

  -- | Finds the first bit on which prefixes disagree.
  commonBranchingBit :: k -> k -> k
  commonBranchingBit p1 p2 = lowesBit (p1 `xor` p2)
    where
      lowesBit :: k -> k
      lowesBit x = x .&. (-x)

  -- | Masks key using supplied branching bit and compares to prefix
  matchPrefix :: k -> k -> k -> Bool
  matchPrefix key prefix bBit = mask key bBit == prefix

  -- | Converts height 'TrieHeight k' into the branching bit 'k'
  heightToBBit :: TrieHeight k -> k
  heightToBBit = (2 ^)

  -- | Converts branching bit 'k' into height 'TrieHeight k'
  bBitToHeight :: k -> TrieHeight k
  bBitToHeight = floor . logBase (2.0 :: Double) . fromIntegral

  -- | Tests whether the desired bit is zero
  zeroBit :: k -> k -> Bool
  zeroBit k m = not (k `testBit` fromIntegral (bBitToHeight m))

-- | Type alias for TrieF
type TrieF' k v = TrieF k (TrieHeight k) v

-- | Type alias for Trie
type Trie k v = Fix (TrieF' k v)

-- | Smart constructor for Branch node
branch :: (TrieKey k, h ~ TrieHeight k) => k -> k -> a -> a -> TrieF k h v a
branch bBit = Branch (bBitToHeight bBit)

-- | Decides how to join two tries based on prefixes(i.e. either as left or right child)
join :: (TrieKey k) => k -> Trie k v -> k -> Trie k v -> Trie k v
join p1 t1 p2 t2 =
  if zeroBit p1 bBit
    then In $ branch bBit (mask p1 bBit) t1 t2
    else In $ branch bBit (mask p1 bBit) t2 t1
  where
    bBit = commonBranchingBit p1 p2
