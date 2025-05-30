{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: This module should only be imported separately wherever it is needed!!!
-- This module contains neccessary instances for the SkyDA types for the offchain part of the app
module Common.OffChain where

import Common
import Data.Aeson
-- import PlutusTx.Prelude (BuiltinByteString)

import Data.ByteString qualified as BS
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Prelude

byteStringToBuiltinByteString :: BS.ByteString -> BuiltinByteString
byteStringToBuiltinByteString = BuiltinByteString

builtinByteStringToByteString :: BuiltinByteString -> BS.ByteString
builtinByteStringToByteString (BuiltinByteString b) = b

-- TODO figure something out for this
instance ToJSON BuiltinByteString where
  -- toJSON = toJSON . builtinByteStringToByteString
  toJSON = undefined

-- TODO and for this as well
instance FromJSON BuiltinByteString where
  -- parseJSON x = parseJSON x >>= return . byteStringToBuiltinByteString
  parseJSON = undefined

deriving via BuiltinByteString instance ToJSON (FixedLengthByteString len)

deriving via BuiltinByteString instance FromJSON (FixedLengthByteString len)

instance ToJSON TopicId where
  toJSON (TopicId tId) = object ["topic_id" .= tId]

instance FromJSON TopicId where
  parseJSON = withObject "TopicId" $ \v -> TopicId <$> v .: "topic_id"

instance FromHttpApiData TopicId where
  parseUrlPiece = undefined

instance ToHttpApiData TopicId where
  toUrlPiece = undefined

instance ToJSON MessageId where
  toJSON (MessageId mId) = object ["message_id" .= mId]

instance FromJSON MessageId where
  parseJSON = withObject "MessageId" $ \v -> MessageId <$> v .: "message_id"

instance FromHttpApiData MessageId where
  parseUrlPiece = undefined

instance ToHttpApiData MessageId where
  toUrlPiece = undefined
