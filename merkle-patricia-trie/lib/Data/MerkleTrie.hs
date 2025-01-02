{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MerkleTrie where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString.Char8 (pack)
import Data.Trie

computeHash :: (Show a) => a -> Digest SHA256
computeHash = hash . pack . show

-- | Compute the hash of a Patricia Trie
-- | For internal use only, since it's partially matched against the trie!
-- TODO don't like the fact, that this is an incomplete pattern, although I can wrap this in Maybe
-- need to think about this
-- | Compute the hash of a Patricia Trie node
computeNodeHash :: (Show k, Show (TrieHeight k), Show v) => TrieF k (TrieHeight k) v (Digest SHA256) -> Digest SHA256
computeNodeHash Empty = computeHash "Empty"
computeNodeHash Leaf {..} = computeHash (key, value)
computeNodeHash Branch {..} = computeHash (height, prefix, left, right)

type MerkleProof = (Digest SHA256, [Digest SHA256])

-- | Generate a Merkle proof for a given key in the trie
-- TODO: check the implementation. Right now it's going through the whole trie and picks the hashes of the nodes
-- that match the key. In the branch, when the branch is one of the branches that lead to the root, it computes
-- the hash using the hash of the leaf and the trie on the other side. Is this right?
-- If not, might need to use paramorphism here, as it allows to see the nodes we're operating on alongside the accumulator.
-- That will allow us to properly calculate the hash of the trie from where we came to the current branch.
proof :: forall k v. (Show k, Show (TrieHeight k), Show v, TrieKey k) => k -> Trie k v -> Maybe MerkleProof
proof k t = cata go t
  where
    go :: Algebra (TrieF' k v) (Maybe MerkleProof)
    go Empty = Nothing
    -- for every leaf we just compute it's hash
    go Leaf {..} = Just (computeHash (key, value), [])
    -- for every branch we look at the prefix
    go Branch {..}
      -- if the key doesn't match the prefix, we calculate the hash and store it
      | not (matchPrefix k prefix (heightToBBit height)) = case (left, right) of
        (Just (hl, _), Just (hr, _)) -> Just (computeHash (height, prefix, hl, hr), [])
        (_, _) -> Nothing
      -- if the key matches the prefix and we're coming from the left branch(zeroBit check),
      -- then we take the hash from there and append the hash of the current node to the path
      | zeroBit k (heightToBBit height) = case (left, right) of
        (Just (hl, p), Just (hr, _)) -> Just (hl, (computeHash (height, prefix, hl, hr)) : p)
        _ -> Nothing
      -- the same here, just for the right branch
      | otherwise = case (left, right) of
        (Just (hl, _), Just (hr, p)) -> Just (hr, (computeHash (height, prefix, hl, hr)) : p)
        _ -> Nothing

-- | Validate a Merkle proof against the root hash of the trie
validate :: Digest SHA256 -> MerkleProof -> Bool
validate rootHash (leafHash, pf) = foldl go leafHash pf == rootHash
  where
    go :: Digest SHA256 -> Digest SHA256 -> Digest SHA256
    go acc h = computeHash (acc, h)
