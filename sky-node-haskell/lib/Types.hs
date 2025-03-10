{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Bits (Bits, FiniteBits)
import Data.ByteString qualified as BS
import Data.Trie
import Data.Word (Word8)
import GHC.Generics

newtype TopicId = TopicId {id :: Int}
  deriving stock (Show, Eq, Generic)
  -- derive THROUGH the newtype using the underlying type
  deriving newtype (Enum, Num, Ord, Real, Bits, FiniteBits, Integral)

instance TrieKey TopicId where
  type TrieHeight TopicId = Word8

newtype TopicMetaData = TopicMetaData {id :: TopicId}
  deriving stock (Show, Eq, Generic)

data Topic = Topic
  { metadata :: TopicMetaData,
    messages :: Trie BlockId BlockData
  }
  deriving stock (Generic)

newtype BlockData = BlockData {blockData :: BS.ByteString}
  deriving stock (Show, Eq, Generic)

newtype BlockId = BlockId {id :: Int}
  deriving stock (Show, Eq, Generic)
  -- derive THROUGH the newtype using the underlying type
  deriving newtype (Enum, Num, Ord, Real, Bits, FiniteBits, Integral)

instance TrieKey BlockId where
  type TrieHeight BlockId = Word8
