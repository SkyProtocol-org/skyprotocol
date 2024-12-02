module Types where

import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)

newtype TopicId = TopicId {topicId :: Int} deriving (Show, Eq)

newtype TopicMetaData = TopicMetaData {topicMetaId :: TopicId} deriving (Show, Eq)

data Topic = Topic
  { topicMeta :: TopicMetaData,
    topicMessages :: IntMap BlockData
  }
  deriving (Show, Eq)

data Block

newtype BlockData = BlockData {blockData :: BS.ByteString} deriving (Show, Eq)

newtype Certificate a = Certificate {cert :: BS.ByteString} deriving (Show, Eq)

-- TODO for now it's a stub
makeBlockCertificate :: BlockData -> Certificate Block
makeBlockCertificate = undefined
