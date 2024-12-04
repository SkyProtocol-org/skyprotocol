module Types where

import Data.Binary
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import GHC.Generics

newtype TopicId = TopicId {id :: Int}
  deriving stock (Show, Eq, Generic)

instance Binary TopicId

newtype TopicMetaData = TopicMetaData {id :: TopicId}
  deriving stock (Show, Eq, Generic)

instance Binary TopicMetaData

data Topic = Topic
  { metadata :: TopicMetaData,
    messages :: IntMap BlockData
  }
  deriving stock (Show, Eq, Generic)

instance Binary Topic

data Block

newtype BlockData = BlockData {blockData :: BS.ByteString}
  deriving stock (Show, Eq, Generic)

instance Binary BlockData

newtype Certificate = Certificate {cert :: BS.ByteString}
  deriving stock (Show, Eq, Generic)

instance Binary Certificate
