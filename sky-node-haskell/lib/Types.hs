{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Crypto.PubKey.RSA
import Data.Aeson
import Data.Bits (Bits, FiniteBits)
import Data.ByteString qualified as BS
import Data.Trie
import Data.WideWord (Word256)
import Data.Word (Word8)
import GHC.Generics

deriving instance ToJSON Word256

deriving instance FromJSON Word256

newtype TopicId = TopicId {topicId :: Word256}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (Enum, Num, Ord, Real, Bits, FiniteBits, Integral)

instance TrieKey TopicId where
  type TrieHeight TopicId = Word8

data TopicMetaData = TopicMetaData
  { topicId :: TopicId,
    committee :: [PublicKey]
  }
  deriving stock (Show, Eq, Generic)

data Topic = Topic
  { metadata :: TopicMetaData,
    messages :: Trie BlockId BlockData
  }
  deriving stock (Generic)

newtype BlockData = BlockData {blockData :: BS.ByteString}
  deriving stock (Show, Eq, Generic)

newtype BlockId = BlockId {id :: Word256}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (Enum, Num, Ord, Real, Bits, FiniteBits, Integral)

instance TrieKey BlockId where
  type TrieHeight BlockId = Word8
