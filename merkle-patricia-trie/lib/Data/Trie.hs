{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trie (Trie (..), TrieKey, lookup, insert) where

import Data.Bits
import Data.Kind (Type)
import Data.Trie.Internal
import Prelude hiding (lookup)

lookup :: (TrieKey k, Functor f) => k -> Trie f k a -> Maybe (f a)
lookup _ Empty = Nothing
lookup k (Leaf key a) = if k == key then Just a else Nothing
lookup k (Branch key prefix t1 t2)
  | not (matchPrefix k prefix key) = Nothing
  | zeroBit k key = lookup k t1
  | otherwise = lookup k t2

-- maybe conflict func must be a Monoid?
insertWith :: (TrieKey k, Functor f, Applicative f) => (v -> v -> v) -> k -> v -> Trie f k v -> Trie f k v
insertWith resolve k v t = go t
  where
    go Empty = Leaf k $ pure v
    go (Leaf key val) =
      if k == key
        then Leaf k $ resolve v <$> val
        else join k (Leaf k $ pure v) key t
    go (Branch key prefix t1 t2) =
      if matchPrefix k prefix key
        then
          if zeroBit k key
            then Branch key prefix (go t1) t2
            else Branch key prefix t1 (go t2)
        else join k (Leaf k $ pure v) prefix t

insert :: (TrieKey k, Functor f, Applicative f) => k -> v -> Trie f k v -> Trie f k v
insert = insertWith const

empty :: (TrieKey k, Functor f) => Trie f k v
empty = Empty
