{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE: This module should only be imported separately wherever it is needed!!!
-- This module contains neccessary instances for the SkyDA types for the offchain part of the app
module Common.OffChain where

import Common
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Text as T
import PlutusTx.Prelude (BuiltinByteString)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (readEither)
import Prelude

instance ToJSON Bytes8 where
  toJSON (Bytes8 s) = toJSON $ toInt s

instance FromJSON Bytes8 where
  parseJSON a = fromInt <$> parseJSON a

instance ToJSON TopicId where
  toJSON (TopicId tId) = object ["topic_id" .= tId]

instance FromJSON TopicId where
  parseJSON = withObject "TopicId" $ \v -> TopicId <$> v .: "topic_id"

instance ToHttpApiData TopicId where
  toUrlPiece (TopicId tId) = pack . show $ toInt tId

instance FromHttpApiData TopicId where
  parseUrlPiece tId = bimap pack (TopicId . fromInt) $ readEither (unpack tId)

instance ToJSON MessageId where
  toJSON (MessageId mId) = object ["message_id" .= mId]

instance FromJSON MessageId where
  parseJSON = withObject "MessageId" $ \v -> MessageId <$> v .: "message_id"

instance ToHttpApiData MessageId where
  toUrlPiece (MessageId mId) = pack . show $ toInt mId

instance FromHttpApiData MessageId where
  parseUrlPiece mId = bimap pack (MessageId . fromInt) $ readEither (unpack mId)

instance ToJSON Bytes32 where
  toJSON (Bytes32 s) = toJSON $ hexOf @BuiltinByteString @Text s

instance FromJSON Bytes32 where
  parseJSON a = Bytes32 . ofHex . unpack <$> parseJSON a

-- TODO: should make conversion to/from hex string
instance FromJSON Hash where
  parseJSON = withObject "Blake2b_256" $ \v -> Blake2b_256 <$> v .: "hash"

instance ToJSON Hash where
  toJSON (Blake2b_256 h) = object ["hash" .= h]
