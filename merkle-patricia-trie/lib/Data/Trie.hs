{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trie (Trie, TrieKey, lookup, insertWith, insert, empty) where

import Data.Trie.Internal
import Prelude hiding (lookup)

-- TODO do we want to unwrap value here or return it wrapped? Maybe 2 different functions for these cases?
lookup :: (TrieKey k) => k -> Trie k a -> Maybe a
lookup _ Empty = Nothing
lookup k Leaf {..} = if k == key then Just value else Nothing
lookup k Branch {..}
  | not (matchPrefix k prefix (heightToBBit branchingBit)) = Nothing
  | zeroBit k (heightToBBit branchingBit) = lookup k left
  | otherwise = lookup k right

insertWith :: forall k a. (TrieKey k) => (a -> a -> a) -> k -> a -> Trie k a -> Trie k a
insertWith resolve k v t = go t
  where
    go :: Trie k a -> Trie k a
    go Empty = Leaf k v
    go Leaf {..} =
      if k == key
        then Leaf k $ resolve v value
        else join k (Leaf k v) key t
    go Branch {..} =
      if matchPrefix k prefix (heightToBBit branchingBit)
        then
          if zeroBit k (heightToBBit branchingBit)
            then Branch branchingBit prefix (go left) right
            else Branch branchingBit prefix left (go right)
        else join k (Leaf k v) prefix t

insert :: (TrieKey k) => k -> v -> Trie k v -> Trie k v
insert = insertWith const

empty :: (TrieKey k) => Trie k v
empty = Empty
