{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.MerkleTrie where

import Control.Applicative ((<|>))
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)

-- | Patricia Trie node
data PatriciaTrie k v = Node
  { prefix :: k,
    value :: Maybe v,
    children :: [PatriciaTrie k v]
  }
  deriving (Show, Generic)

-- | Merkle Patricia Trie
data MerklePatriciaTrie k v = MerklePatriciaTrie
  { rootHash :: Digest SHA256,
    rootNode :: PatriciaTrie k v
  }
  deriving (Show, Generic)

-- | Compute the hash of a Patricia Trie node
computeNodeHash :: (Show k, Show v) => PatriciaTrie k v -> Digest SHA256
computeNodeHash Node {..} =
  hash . pack $ show (prefix, value, map computeNodeHash children)

-- | Update the root hash of the Merkle Trie
updateRootHash :: (Show k, Show v) => PatriciaTrie k v -> Digest SHA256
updateRootHash = computeNodeHash

-- | Create an empty Merkle Patricia Trie
emptyTrie :: forall k v. (Show k, Show v) => MerklePatriciaTrie [k] v
emptyTrie =
  MerklePatriciaTrie
    { rootHash = computeNodeHash @[k] @v root,
      rootNode = root
    }
  where
    root = Node {prefix = [], value = Nothing, children = []}

-- | Create a singleton Patricia Trie
singletonTrie :: forall k v. (Show k, Show v, Eq k) => [k] -> v -> MerklePatriciaTrie [k] v
singletonTrie k v =
  MerklePatriciaTrie
    { rootHash = computeNodeHash @[k] @v root,
      rootNode = root
    }
  where
    root = Node {prefix = k, value = Just v, children = []}

-- | Insert a key-value pair into a Patricia Trie node
insertNode :: (Eq k, Ord k) => [k] -> v -> PatriciaTrie [k] v -> PatriciaTrie [k] v
insertNode [] v node = node {value = Just v}
insertNode key v Node {..}
  | prefix `isPrefixOf` key =
      let remaining = drop (length prefix) key
       in if null remaining
            then Node {value = Just v, ..}
            else Node {children = insertChild remaining v children, ..}
  | otherwise = mergeNodes key v prefix value children
  where
    insertChild [] val [] = [Node {prefix = [], value = Just val, children = []}]
    insertChild r val (c : cs)
      | commonPrefix r c.prefix /= [] =
          let splitChild = insertNode (drop (length (commonPrefix r c.prefix)) r) val c
           in splitChild : cs
      | otherwise = c : insertChild r val cs
    insertChild r val [] = [Node {prefix = r, value = Just val, children = []}]
    mergeNodes newKey newValue oldKey oldValue oldChildren =
      let common = commonPrefix newKey oldKey
          splitOld = Node {prefix = drop (length common) oldKey, value = oldValue, children = oldChildren}
          splitNew = Node {prefix = drop (length common) newKey, value = Just newValue, children = []}
       in Node {prefix = common, value = Nothing, children = [splitOld, splitNew]}
    commonPrefix :: (Eq a) => [a] -> [a] -> [a]
    commonPrefix [] _ = []
    commonPrefix _ [] = []
    commonPrefix (x : xs) (y : ys)
      | x == y = x : commonPrefix xs ys
      | otherwise = []

-- | Insert into the Merkle Patricia Trie
insertTrie :: (Show k, Ord k, Eq k, Show v) => [k] -> v -> MerklePatriciaTrie [k] v -> MerklePatriciaTrie [k] v
insertTrie k v MerklePatriciaTrie {rootNode} =
  let newRoot = insertNode k v rootNode
   in MerklePatriciaTrie {rootHash = updateRootHash newRoot, rootNode = newRoot}

-- | Generate a Merkle proof for a key
merkleProof :: forall k v. (Eq k, Show k, Show v) => [k] -> PatriciaTrie [k] v -> Maybe (v, Digest SHA256)
merkleProof [] Node {value} = (\v -> (v, computeNodeHash @[k] @v Node {prefix = [], value = Just v, children = []})) <$> value
merkleProof key Node {prefix, children}
  | prefix `isPrefixOf` key =
      let remaining = drop (length prefix) key
       in foldr (\child acc -> acc <|> merkleProof remaining child) Nothing children
  | otherwise = Nothing
