{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Merkle.Types where

import Crypto.Hash (Blake2b_256, Digest, digestFromByteString, hashlazy)
import Data.Aeson hiding (decode, encode)
import Data.Binary
import Data.Bits
import Data.ByteArray qualified as BA (unpack)
import Data.ByteString qualified as BSS (ByteString, pack)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Char (chr, ord, toLower)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Trie
import GHC.Generics (Generic)
import Numeric (showHex)

data MerkleTrie k v = MerkleTrie
  { rootHash :: Digest Blake2b_256,
    trie :: Trie k v
  }

merkelize :: (Binary k, Binary v, Binary (TrieHeight k)) => Trie k v -> MerkleTrie k v
merkelize trie = let rootHash = computeRootHash trie in MerkleTrie rootHash trie

data ProofWithRootHash = MkProofWithRoot {root :: T.Text, proofData :: MerkleProof}
  deriving (Show, Eq, Generic)

mkProofWithHash :: Digest Blake2b_256 -> MerkleProof -> ProofWithRootHash
mkProofWithHash h = MkProofWithRoot (T.pack . hex $ chr . fromIntegral <$> BA.unpack h)

instance ToJSON ProofWithRootHash where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ProofWithRootHash

data MerkleProof = MerkleProof
  { targetKey :: Integer,
    targetValue :: BS.ByteString,
    keySize :: Int,
    keyPath :: [Integer],
    siblingHashes :: [Digest Blake2b_256]
  }
  deriving (Eq, Show, Generic)

instance ToJSON MerkleProof where
  toJSON MerkleProof {..} =
    object
      [ "alternative" .= (0 :: Word8),
        "fields"
          .= [ toJSON . T.pack . (\r -> if even $ length r then r else '0' : r) $ showHex targetKey "",
               toJSON . T.pack . hex $ L.unpack targetValue,
               toJSON keySize,
               toJSON keyPath,
               toJSON $ T.pack . hex . (chr . fromIntegral <$>) <$> (BA.unpack <$> siblingHashes)
             ]
      ]

instance FromJSON MerkleProof where
  parseJSON (Object v) = do
    [tKey, tValue, kSize, kPath, sHashes] <- v .: "fields"
    targetKey <- decode . BS.pack <$> parseJSON tKey
    targetValue <- BS.pack <$> parseJSON tValue
    keySize <- parseJSON kSize
    keyPath <- parseJSON kPath
    digests <- parseJSON sHashes
    let siblingHashes = fromJust . digestFromByteString . BSS.pack <$> digests
    pure MerkleProof {..}

instance Binary MerkleProof where
  put MerkleProof {..} = do
    put ("proof" :: String)
    put targetKey
    put targetValue
    put keySize
    putList keyPath
    putList $ BS.pack . BA.unpack <$> siblingHashes
  get = do
    iden <- get
    case iden of
      ("proof" :: String) -> MerkleProof <$> get <*> get <*> get <*> get <*> getSiblingHashes
    where
      getSiblingHashes :: Get [Digest Blake2b_256]
      getSiblingHashes = do
        digests <- get :: Get [BSS.ByteString]
        pure $ case mapM digestFromByteString digests of
          Just a -> a
          Nothing -> error "Can't deserialize siblingHashes"

-- Some utils functions

computeHash :: (Binary a) => a -> Digest Blake2b_256
computeHash = hashlazy . encode

computeHashAsBS :: (Binary a) => a -> BS.ByteString
computeHashAsBS = BS.pack . BA.unpack . computeHash

computeRootHash :: forall k v. (Binary k, Binary (TrieHeight k), Binary v) => Trie k v -> Digest Blake2b_256
computeRootHash = cata go
  where
    go :: Algebra (TrieF' k v) (Digest Blake2b_256)
    go Empty = computeHash (0 :: Word8)
    go Leaf {..} = computeHash (1 :: Word8, value)
    go Branch {..} = hashlazy $ computeHashAsBS (2 :: Word8) <> BS.pack (BA.unpack left) <> BS.pack (BA.unpack right)

-- | Utility typeclass to convert values to hex representation
class Hex t where
  hex :: t -> t
  unhex :: t -> Either String t

-- | Utility function to map hex char to it's int representation
c :: Char -> Either String Int
c ch
  | ord ch <= 57 = pure $ ord ch - 48
  | ord (toLower ch) <= 102 = pure $ ord (toLower ch) - 102 + 10
  | otherwise = Left "Invalid hex digit!"

instance Hex String where
  hex = concatMap w
    where
      w ch =
        let s = "0123456789ABCDEF"
            x = fromEnum ch
         in [s !! div x 16, s !! mod x 16]
  unhex [] = return []
  unhex (a : b : r) = do
    x <- c a
    y <- c b
    (toEnum ((x * 16) + y) :) <$> unhex r
  unhex [_] = Left "Non-even length"

instance Hex B.ByteString where
  hex = B.pack . hex . B.unpack
  unhex x = B.pack <$> unhex (B.unpack x)

instance Hex L.ByteString where
  hex = L.pack . hex . L.unpack
  unhex x = L.pack <$> unhex (L.unpack x)
