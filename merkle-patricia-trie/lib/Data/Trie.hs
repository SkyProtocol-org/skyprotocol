{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trie (LittleTrie, BigTrie, TrieKey, lookup, insert, empty) where

import Data.Trie.Internal
import Data.Utils
import Prelude hiding (lookup)

-- TODO do we want to unwrap value here or return it wrapped? Maybe 2 different functions for these cases?
lookup :: (TrieKey k, Wrapper w) => k -> Trie w k a -> Maybe (w a)
lookup _ Empty = Nothing
lookup k (Leaf key a) = if k == key then Just a else Nothing
lookup k (Branch bBit prefix t1 t2)
  | not (matchPrefix k prefix bBit) = Nothing
  | zeroBit k bBit = lookup k t1
  | otherwise = lookup k t2

insertWith :: (TrieKey k, Wrapper w) => (v -> v -> v) -> k -> v -> Trie w k v -> Trie w k v
insertWith resolve k v t = go t
  where
    go Empty = Leaf k $ pure v
    go (Leaf key val) =
      if k == key
        then Leaf k $ resolve v <$> val
        else join k (Leaf k $ pure v) key t
    go (Branch bBit prefix t1 t2) =
      if matchPrefix k prefix bBit
        then
          if zeroBit k bBit
            then Branch bBit prefix (go t1) t2
            else Branch bBit prefix t1 (go t2)
        else join k (Leaf k $ pure v) prefix t

insert :: (TrieKey k, Wrapper w) => k -> v -> Trie w k v -> Trie w k v
insert = insertWith const

empty :: (TrieKey k, Wrapper w) => Trie w k v
empty = Empty
