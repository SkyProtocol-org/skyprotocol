{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Merkle.Trie
  ( proof,
    proofWith,
    validate,
    merkelize,
    merkelizeWith,
    MerkleProof (..),
    MerkleTrie (..),
    mkProofWithHash,
  )
where

import Crypto.Hash (Blake2b_256, Digest, hashlazy)
import Data.Binary (Binary, encode)
import Data.Bits
import Data.ByteArray qualified as BA (unpack)
import Data.ByteString.Lazy qualified as BS
import Data.Trie
import Data.Word (Word8)
import Merkle.Types

-- | Generate a Merkle proof for a given key in the trie
-- Yeah, this variant always constructs whole proof even if the key is not there
proofWith :: forall k v a. (TrieKey k, FiniteBits k, Binary a, Binary k) => (v -> Maybe a) -> (v -> Digest Blake2b_256) -> k -> Trie k v -> Maybe MerkleProof
proofWith f fh k t = let r = cata go t in if fst r then snd r else Nothing
  where
    keySize = finiteBitSize k
    -- Bool to signify if the key was in the structure
    go :: Algebra (TrieF' k v) (Bool, Maybe MerkleProof)
    go Empty = (False, Nothing)
    -- for every leaf we just compute merkle proof
    go Leaf {..} =
      let targetKey = fromIntegral key
          targetValue = encode <$> f value
          targetHash = fh value
          keyPath = []
          siblingHashes = [computeHash (1 :: Word8, BS.pack . BA.unpack $ fh value) | key /= k]
       in (key == k, Just $ MerkleProof {..})
    go (Branch h _ (bl, Just pl) (br, Just pr))
      -- if one of the proofs containt the key, it means it comes from a path that we're interested in
      -- it also means, that this node is in the path
      | pl.targetKey == fromIntegral k =
          let targetKey = pl.targetKey
              targetValue = pl.targetValue
              targetHash = pl.targetHash
              keyPath = fromIntegral h : pl.keyPath
              siblingHashes = pr.siblingHashes <> pl.siblingHashes
           in (bl, Just $ MerkleProof {..})
      | pr.targetKey == fromIntegral k =
          let targetKey = pr.targetKey
              targetValue = pr.targetValue
              targetHash = pr.targetHash
              keyPath = fromIntegral h : pr.keyPath
              siblingHashes = pr.siblingHashes <> pl.siblingHashes
           in (br, Just $ MerkleProof {..})
      -- otherwise it doesn't matter what key we pick, we're interested only in hash
      | otherwise =
          let targetKey = pl.targetKey
              targetValue = pl.targetValue
              targetHash = pl.targetHash
              keyPath = []
              -- in this case there is always one element in the path list
              siblingHashes =
                [ hashlazy $
                    computeHashAsBS (2 :: Word8)
                      <> BS.pack (BA.unpack (head pl.siblingHashes))
                      <> BS.pack (BA.unpack (head pr.siblingHashes))
                ]
           in (False, Just $ MerkleProof {..})
    go Branch {} = (False, Nothing)

proof :: forall k v. (TrieKey k, FiniteBits k, Binary v, Binary k) => k -> Trie k v -> Maybe MerkleProof
proof = proofWith Just computeHash

-- | Validate a Merkle proof against the root hash of the trie
validate :: MerkleProof -> Digest Blake2b_256 -> Bool
validate MerkleProof {..} rootHash =
  valid
    && rootHash
      == foldr
        ( \(h, hs) acc ->
            if not (targetKey `testBit` fromIntegral h)
              then hashlazy $ computeHashAsBS (2 :: Word8) <> BS.pack (BA.unpack acc) <> BS.pack (BA.unpack hs)
              else hashlazy $ computeHashAsBS (2 :: Word8) <> BS.pack (BA.unpack hs) <> BS.pack (BA.unpack acc)
        )
        (hashlazy $ encode (1 :: Word8) <> val)
        (reverse $ zip keyPath siblingHashes)
  where
    -- if targetValue is present, check that targetHash equals hashed targetValue
    valid = case targetValue of
      Just v -> hashlazy @Blake2b_256 (encode v) == targetHash
      Nothing -> True
    val = case targetValue of
      Just v -> v
      Nothing -> BS.pack $ BA.unpack targetHash
