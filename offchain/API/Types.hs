{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types where

import Common
import Common.OffChain ()
import Data.Aeson
import Data.ByteString qualified as BS
import Data.OpenApi
import GHC.Generics
import GeniusYield.Types
import Servant
import Servant.OpenApi

newtype User = User
  { userEmail :: BS.ByteString
  }

-- TODO: all the change addr, used addrs and collateral are unnecessary
-- if we don't do this from one node.
-- We want to have one node - one user, which will allow us to persist these
data CreateBridgeRequest = CreateBridgeRequest
  { -- | Address to where left-overs are returned
    cbrChangeAddr :: GYAddress,
    -- | Used addresses for compatibility with non-single address wallets
    cbrUsedAddrs :: [GYAddress],
    cbrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON CreateBridgeRequest where
  parseJSON = withObject "CreateBridgeRequest" $ \v -> do
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    cbrCollateral <- v .:? "collateral"
    let cbrChangeAddr = unsafeAddressFromText changeAddr
        cbrUsedAddrs = fmap unsafeAddressFromText usedAddrs
    pure CreateBridgeRequest {..}

instance ToJSON CreateBridgeRequest where
  toJSON CreateBridgeRequest {..} =
    object
      [ "changeAddr" .= addressToText cbrChangeAddr,
        "usedAddrs" .= fmap addressToText cbrUsedAddrs,
        "collateral" .= cbrCollateral
      ]

data UpdateBridgeRequest = UpdateBridgeRequest
  { ubrChangeAddr :: GYAddress,
    ubrUsedAddrs :: [GYAddress],
    ubrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON UpdateBridgeRequest where
  parseJSON = withObject "UpdateBridgeRequest" $ \v -> do
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    ubrCollateral <- v .:? "collateral"
    let ubrChangeAddr = unsafeAddressFromText changeAddr
        ubrUsedAddrs = fmap unsafeAddressFromText usedAddrs
    pure UpdateBridgeRequest {..}

instance ToJSON UpdateBridgeRequest where
  toJSON UpdateBridgeRequest {..} =
    object
      [ "changeAddr" .= addressToText ubrChangeAddr,
        "usedAddrs" .= fmap addressToText ubrUsedAddrs,
        "collateral" .= ubrCollateral
      ]

-- instance FromJSON CreateBridgeRequest where
--   parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "cbr"}

data OfferBountyRequest = OfferBountyRequest
  { obrTopicId :: TopicId,
    obrMessageHash :: Hash,
    obrAmount :: Integer,
    -- | Number of slots from the current
    obrDeadline :: Integer,
    obrChangeAddr :: GYAddress,
    obrUsedAddrs :: [GYAddress],
    obrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON OfferBountyRequest where
  parseJSON = withObject "OfferBountyRequest" $ \v -> do
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    obrCollateral <- v .:? "collateral"
    obrTopicId <- v .: "topicId"
    obrDeadline <- v .: "deadline"
    obrMessageHash <- v .: "messageHash"
    obrAmount <- v .: "amount"
    let obrChangeAddr = unsafeAddressFromText changeAddr
        obrUsedAddrs = fmap unsafeAddressFromText usedAddrs
    pure OfferBountyRequest {..}

instance ToJSON OfferBountyRequest where
  toJSON OfferBountyRequest {..} =
    object
      [ "topicId" .= obrTopicId,
        "messageHash" .= obrMessageHash,
        "deadline" .= obrDeadline,
        "changeAddr" .= addressToText obrChangeAddr,
        "usedAddrs" .= fmap addressToText obrUsedAddrs,
        "collateral" .= obrCollateral,
        "amount" .= obrAmount
      ]

-- TODO: save contract addresses and hashes in some cache, to get rid of
-- unnecessary request parameters
-- (why claimant must know about the deadline?)
data ClaimBountyRequest = ClaimBountyRequest
  { cbrTopicId :: TopicId,
    cbrMessageId :: MessageId,
    cbrMessageHash :: Hash,
    -- | Number of slots from the current. Should be the same as in OfferBountyRequest
    cbrDeadline :: Integer,
    -- | The slot where the deadline was started
    cbrDeadlineStart :: Integer,
    cBountyrChangeAddr :: GYAddress,
    cBountyrUsedAddrs :: [GYAddress],
    cBountyrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON ClaimBountyRequest where
  parseJSON = withObject "ClaimBountyRequest" $ \v -> do
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    cBountyrCollateral <- v .:? "collateral"
    cbrTopicId <- v .: "topicId"
    cbrMessageId <- v .: "messageId"
    cbrDeadline <- v .: "deadline"
    cbrDeadlineStart <- v .: "deadlineStart"
    cbrMessageHash <- v .: "messageHash"
    let cBountyrChangeAddr = unsafeAddressFromText changeAddr
        cBountyrUsedAddrs = fmap unsafeAddressFromText usedAddrs
    pure ClaimBountyRequest {..}

instance ToJSON ClaimBountyRequest where
  toJSON ClaimBountyRequest {..} =
    object
      [ "topicId" .= cbrTopicId,
        "messageId" .= cbrMessageId,
        "messageHash" .= cbrMessageHash,
        "deadline" .= cbrDeadline,
        "deadlineStart" .= cbrDeadlineStart,
        "changeAddr" .= addressToText cBountyrChangeAddr,
        "usedAddrs" .= fmap addressToText cBountyrUsedAddrs,
        "collateral" .= cBountyrCollateral
      ]

newtype ProofBytes = ProofBytes {getProofBytes :: BS.ByteString}
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON, Eq, Show)

instance ToSchema ProofBytes where
  declareNamedSchema _ = pure $ (NamedSchema $ Just "ProofBytes") binarySchema

instance MimeRender OctetStream ProofBytes where
  mimeRender _ (ProofBytes bs) = BS.fromStrict bs

instance MimeUnrender OctetStream ProofBytes where
  mimeUnrender _ bs = Right $ ProofBytes $ BS.toStrict bs

newtype RawBytes = RawBytes {getRawBytes :: BS.ByteString}
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON, Eq, Show)

instance ToSchema RawBytes where
  declareNamedSchema _ = pure $ (NamedSchema $ Just "RawBytes") binarySchema

instance MimeRender OctetStream RawBytes where
  mimeRender _ (RawBytes bs) = BS.fromStrict bs

instance MimeUnrender OctetStream RawBytes where
  mimeUnrender _ bs = Right $ RawBytes $ BS.toStrict bs

instance (HasOpenApi api) => HasOpenApi (BasicAuth realm user :> api) where
  toOpenApi _ = mempty
