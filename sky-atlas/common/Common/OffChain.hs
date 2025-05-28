-- NOTE: This module should only be imported separately wherever it is needed!!!
-- This module contains neccessary instances for the SkyDA types for the offchain part of the app
{-# OPTIONS_GHC -Wno-orphans #-}

module Common.OffChain where

import Common
import Data.Aeson
import PlutusTx.Prelude (BuiltinByteString)
import Prelude

-- TODO figure something out for this
instance ToJSON BuiltinByteString where
  toJSON _bs = undefined

instance FromJSON BuiltinByteString where
  parseJSON = undefined

deriving via BuiltinByteString instance (ToJSON Bytes8)

deriving via BuiltinByteString instance (FromJSON Bytes8)

instance ToJSON TopicId where
  toJSON (TopicId tId) = object ["topic_id" .= tId]

instance FromJSON TopicId where
  parseJSON = withObject "TopicId" $ \v -> TopicId <$> v .: "topic_id"

instance ToJSON MessageId where
  toJSON (MessageId mId) = object ["message_id" .= mId]

instance FromJSON MessageId where
  parseJSON = withObject "MessageId" $ \v -> MessageId <$> v .: "message_id"
