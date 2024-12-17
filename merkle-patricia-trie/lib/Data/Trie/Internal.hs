{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trie.Internal where

import Data.Bits
import Data.Kind (Type)
import Data.WideWord (Word256)
import Data.Word (Word8)
import Prelude hiding (lookup)

data Trie' k h a
  = Empty
  | Leaf {key :: k, val :: a}
  | Branch {bBit :: h, pref :: k, left :: Trie' k h a, right :: Trie' k h a}
  deriving (Show, Eq, Functor)

class (Bits k, Num k, Integral k) => TrieKey k where
  -- | This will allow us to enforce the height of the trie depending on it's key
  type TrieHeight k :: Type

  -- | Discards bits before/after(depending on endianess) branching bit
  mask :: k -> k -> k

  -- | Leaves only the branching bit(so key 0b1001101 becomes 0b0001000, if 4th bit was a branching bit)
  branchingBit :: k -> k -> k

  matchPrefix :: k -> k -> k -> Bool
  matchPrefix bBit prefix key = mask bBit key == prefix

  zeroBit :: k -> k -> Bool
  zeroBit a b = (a .&. b) == 0

join :: (TrieKey k) => k -> Trie' k h a -> k -> Trie' k h a -> Trie' k h a
join p1 t1 p2 t2 =
  if zeroBit p1 bBit
    then Branch bBit (mask p1 bBit) t1 t2
    else Branch bBit (mask p1 bBit) t2 t1
  where
    bBit = branchingBit p1 p2
