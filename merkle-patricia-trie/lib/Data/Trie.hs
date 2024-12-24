{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trie (Trie' (..), Trie, TrieKey (..), lookup, insertWith, insert, singleton, empty, module X) where

import Data.Trie.Internal
import Data.Trie.Zipper as X -- importing like this to re-export
import Prelude hiding (lookup)

-- TODO do we want to unwrap value here or return it wrapped? Maybe 2 different functions for these cases?
lookup :: (TrieKey k) => k -> Trie k a -> Maybe a
lookup _ Empty = Nothing
lookup k Leaf {..} = if k == key then Just value else Nothing
lookup k Branch {..}
  | not (matchPrefix k prefix (heightToBBit height)) = Nothing
  | zeroBit k (heightToBBit height) = lookup k left
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
      if matchPrefix k prefix (heightToBBit height)
        then
          if zeroBit k (heightToBBit height)
            then Branch height prefix (go left) right
            else Branch height prefix left (go right)
        else join k (Leaf k v) prefix t

insert :: (TrieKey k) => k -> v -> Trie k v -> Trie k v
insert = insertWith const

singleton :: (TrieKey k) => k -> a -> Trie k a
singleton k v = insert k v empty

empty :: (TrieKey k) => Trie k a
empty = Empty
