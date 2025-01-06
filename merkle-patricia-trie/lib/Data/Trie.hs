{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Trie
  ( TrieF (..),
    TrieF',
    Trie,
    TrieKey (..),
    prettyPrint,
    lookup,
    insertWith,
    insert,
    singleton,
    empty,
    module X,
  )
where

import Data.Internal.RecursionSchemes as X
import Data.Internal.Trie
import Prelude hiding (lookup)

prettyPrint :: forall k v. (TrieKey k, Show k, Show (TrieHeight k), Show v) => Trie k v -> String
prettyPrint = cata go
  where
    go :: Algebra (TrieF' k v) String
    go Empty = "()"
    go Leaf {..} = "(" <> show key <> ", " <> show value <> ")"
    go Branch {..} =
      "{"
        <> show height
        <> ", "
        <> show prefix
        <> "}\n"
        <> "/"
        <> replicate 10 ' '
        <> "\\\n"
        <> left
        <> replicate 10 ' '
        <> right

lookup :: (TrieKey k) => k -> Trie k v -> Maybe v
lookup _ (In Empty) = Nothing
lookup k (In Leaf {..}) = if k == key then Just value else Nothing
lookup k (In Branch {..})
  | not (matchPrefix k prefix (heightToBBit height)) = Nothing
  | zeroBit k (heightToBBit height) = lookup k left
  | otherwise = lookup k right

-- | Inserts new value in the trie resolving possible conflicts using `(v -> v -> v)`.
-- | Values are supplied in the following order: new value -> old value.
insertWith :: forall k v. (TrieKey k) => (v -> v -> v) -> k -> v -> Trie k v -> Trie k v
insertWith resolve k v t = go t
  where
    go :: Trie k v -> Trie k v
    go (In Empty) = In $ Leaf k v
    go (In Leaf {..}) =
      if k == key
        then In $ Leaf k $ resolve v value
        else join k (In $ Leaf k v) key t
    go (In Branch {..}) =
      if matchPrefix k prefix $ heightToBBit height
        then
          if zeroBit k $ heightToBBit height
            then In $ Branch height prefix (go left) right
            else In $ Branch height prefix left (go right)
        else join k (In $ Leaf k v) prefix t

insert :: forall k v. (TrieKey k) => k -> v -> Trie k v -> Trie k v
insert = insertWith const

singleton :: forall k v. (TrieKey k) => k -> v -> Trie k v
singleton k v = insert k v empty

empty :: forall k v. (TrieKey k) => Trie k v
empty = In Empty
