{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MerkleTrie
  ( proof,
    validate,
    computeRootHash,
    merkelize,
    MerkleProof (..),
    MerkleTrie (..),
  )
where

import Crypto.Hash (Blake2b_256, Digest, hash)
import Data.ByteString.Char8 (pack)
import Data.Trie

data MerkleTrie k v = MerkleTrie
  { rootHash :: Digest Blake2b_256,
    trie :: Trie k v
  }

computeRootHash :: forall k v. (Show k, Show v, Show (TrieHeight k)) => Trie k v -> Maybe (Digest Blake2b_256)
computeRootHash = cata go
  where
    go :: Algebra (TrieF' k v) (Maybe (Digest Blake2b_256))
    go Empty = Nothing
    go Leaf {..} = Just $ computeHash (key, value)
    go Branch {..} = Just $ computeHash (height, prefix, left, right)

merkelize :: (Show k, Show v, Show (TrieHeight k)) => Trie k v -> Maybe (MerkleTrie k v)
merkelize trie = let rootHash = computeRootHash trie in flip MerkleTrie trie <$> rootHash

computeHash :: (Show a) => a -> Digest Blake2b_256
computeHash = hash . pack . show

data MerkleProof k v = MerkleProof
  { targetKey :: k,
    targetValue :: v,
    keyPath :: [(TrieHeight k, k)],
    siblingHashes :: [Digest Blake2b_256]
  }

-- | Generate a Merkle proof for a given key in the trie
-- Yeah, this variant always constructs whole proof even if the key is not there
proof :: forall k v. (Show k, Show (TrieHeight k), Show v, TrieKey k) => k -> Trie k v -> Maybe (MerkleProof k v)
proof k t = let r = para go t in if fst r then snd r else Nothing
  where
    -- Bool to signify if the key was in the structure
    go :: RAlgebra (TrieF' k v) (Bool, Maybe (MerkleProof k v))
    go Empty = (False, Nothing)
    -- for every leaf we just compute merkle proof
    go Leaf {..} =
      let targetKey = k
          targetValue = value
          keyPath = []
          siblingHashes = [computeHash (key, value)]
       in (targetKey == k, Just $ MerkleProof {..})
    go (Branch h p (_, (bl, Just pl)) (_, (br, Just pr)))
      -- if one of the proofs containt the key, it means it comes from a path that we're interested in
      -- it also means, that this node is in the path
      | pl.targetKey == k =
          let targetKey = pl.targetKey
              targetValue = pl.targetValue
              keyPath = (h, p) : pl.keyPath
              siblingHashes = pr.siblingHashes <> pl.siblingHashes
           in (bl, Just $ MerkleProof {..})
      | pr.targetKey == k =
          let targetKey = pr.targetKey
              targetValue = pr.targetValue
              keyPath = (h, p) : pr.keyPath
              siblingHashes = pl.siblingHashes <> pr.siblingHashes
           in (br, Just $ MerkleProof {..})
      -- otherwise it doesn't matter what key we pick, we're interested only in hash
      | otherwise =
          let targetKey = pl.targetKey
              targetValue = pl.targetValue
              keyPath = []
              -- in this case there is always one element in the path list
              siblingHashes = [computeHash (h, p, head pl.siblingHashes, head pr.siblingHashes)]
           in (False, Just $ MerkleProof {..})
    go Branch {} = (False, Nothing)

-- | Validate a Merkle proof against the root hash of the trie
validate :: (Show k, Show (TrieHeight k), Show v) => MerkleProof k v -> Digest Blake2b_256 -> Bool
validate MerkleProof {..} rootHash =
  rootHash
    == ( foldr
           (\((h, p), hs) acc -> computeHash (h, p, hs, acc))
           (computeHash (targetKey, targetValue))
           . reverse
           $ zip keyPath siblingHashes
       )
