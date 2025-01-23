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

import Crypto.Hash (Blake2b_256, Digest, digestFromByteString, hashlazy)
import Data.Binary
import Data.Bits
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BS
import Data.Trie

data MerkleTrie k v = MerkleTrie
  { rootHash :: Digest Blake2b_256,
    trie :: Trie k v
  }

data MerkleProof = MerkleProof
  { targetKey :: Integer,
    targetValue :: BS.ByteString,
    keySize :: Int,
    keyPath :: [Integer],
    siblingHashes :: [Digest Blake2b_256]
  }
  deriving (Eq, Show)

instance Binary MerkleProof where
  put MerkleProof {..} = do
    put "proof"
    put targetKey
    put targetValue
    put keySize
    putList keyPath
    putList $ BS.pack . BA.unpack <$> siblingHashes
  get = do
    iden <- get
    case iden of
      "proof" -> MerkleProof <$> get <*> get <*> get <*> get <*> getSiblingHashes
    where
      getSiblingHashes :: Get [Digest Blake2b_256]
      getSiblingHashes = do
        digests <- get :: Get [BSS.ByteString]
        pure $ case mapM digestFromByteString digests of
          Just a -> a
          Nothing -> error "Can't deserialize siblingHashes"

-- TODO: replace the 'show' with binary serialization
computeHash :: (Binary a) => a -> Digest Blake2b_256
computeHash = hashlazy . encode

computeHashAsBS :: (Binary a) => a -> BS.ByteString
computeHashAsBS = BS.pack . BA.unpack . computeHash

computeRootHash :: forall k v. (Binary k, Binary (TrieHeight k), Binary v) => Trie k v -> Digest Blake2b_256
computeRootHash = cata go
  where
    go :: Algebra (TrieF' k v) (Digest Blake2b_256)
    go Empty = computeHash (0 :: Int)
    go Leaf {..} = computeHash (1 :: Int, value)
    go Branch {..} = hashlazy $ computeHashAsBS (2 :: Int) <> BS.pack (BA.unpack left) <> BS.pack (BA.unpack right)

merkelize :: (Binary k, Binary v, Binary (TrieHeight k)) => Trie k v -> MerkleTrie k v
merkelize trie = let rootHash = computeRootHash trie in MerkleTrie rootHash trie

-- | Generate a Merkle proof for a given key in the trie
-- Yeah, this variant always constructs whole proof even if the key is not there
proof :: forall k v. (TrieKey k, FiniteBits k, Binary v, Binary k, Binary (TrieHeight k)) => k -> Trie k v -> Maybe MerkleProof
proof k t = let r = cata go t in if fst r then snd r else Nothing
  where
    keySize = finiteBitSize k
    -- Bool to signify if the key was in the structure
    go :: Algebra (TrieF' k v) (Bool, Maybe MerkleProof)
    go Empty = (False, Nothing)
    -- for every leaf we just compute merkle proof
    go Leaf {..} =
      let targetKey = fromIntegral key
          targetValue = encode value
          keyPath = []
          siblingHashes = [computeHash (1 :: Int, value) | key /= k]
       in (key == k, Just $ MerkleProof {..})
    go (Branch h _ (bl, Just pl) (br, Just pr))
      -- if one of the proofs containt the key, it means it comes from a path that we're interested in
      -- it also means, that this node is in the path
      | pl.targetKey == fromIntegral k =
          let targetKey = pl.targetKey
              targetValue = pl.targetValue
              keyPath = fromIntegral h : pl.keyPath
              siblingHashes = pr.siblingHashes <> pl.siblingHashes
           in (bl, Just $ MerkleProof {..})
      | pr.targetKey == fromIntegral k =
          let targetKey = pr.targetKey
              targetValue = pr.targetValue
              keyPath = fromIntegral h : pr.keyPath
              siblingHashes = pr.siblingHashes <> pl.siblingHashes
           in (br, Just $ MerkleProof {..})
      -- otherwise it doesn't matter what key we pick, we're interested only in hash
      | otherwise =
          let targetKey = pl.targetKey
              targetValue = pl.targetValue
              keyPath = []
              -- in this case there is always one element in the path list
              siblingHashes =
                [ hashlazy $
                    computeHashAsBS (2 :: Int)
                      <> BS.pack (BA.unpack (head pl.siblingHashes))
                      <> BS.pack (BA.unpack (head pr.siblingHashes))
                ]
           in (False, Just $ MerkleProof {..})
    go Branch {} = (False, Nothing)

-- | Validate a Merkle proof against the root hash of the trie
validate :: MerkleProof -> Digest Blake2b_256 -> Bool
validate MerkleProof {..} rootHash =
  rootHash
    == foldr
      ( \(h, hs) acc ->
          if not (targetKey `testBit` fromIntegral h)
            then hashlazy $ computeHashAsBS (2 :: Int) <> BS.pack (BA.unpack acc) <> BS.pack (BA.unpack hs)
            else hashlazy $ computeHashAsBS (2 :: Int) <> BS.pack (BA.unpack hs) <> BS.pack (BA.unpack acc)
      )
      (hashlazy $ encode (1 :: Int) <> targetValue)
      (reverse $ zip keyPath siblingHashes)
