module Types where

import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)

newtype TopicId = TopicId {id :: Int} deriving (Show, Eq)

newtype TopicMetaData = TopicMetaData {id :: TopicId} deriving (Show, Eq)

data Topic = Topic
  { metadata :: TopicMetaData,
    messages :: IntMap BlockData
  }
  deriving (Show, Eq)

data Block

newtype BlockData = BlockData {blockData :: BS.ByteString} deriving (Show, Eq)

newtype Certificate a = Certificate {cert :: BS.ByteString} deriving (Show, Eq)

-- TODO for now it's a stub
makeBlockCertificate :: BlockData -> Certificate Block
makeBlockCertificate = undefined
