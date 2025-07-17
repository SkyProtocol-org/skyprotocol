module API.Types where

import Common
import Common.OffChain ()
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import GeniusYield.Types

newtype User = User
  { userEmail :: BS.ByteString
  }

-- TODO: all the change addr, used addrs and collateral are unnecessary
-- if we don't do this from one node.
-- We want to have one node - one user, which will allow us to persist these
data CreateBridgeRequest = CreateBridgeRequest
  { -- | Optional amount of tokens to mint defaults to 1
    cbrAmount :: Maybe Integer,
    -- | wtf is this?
    cbrChangeAddr :: GYAddress,
    -- | Used addresses for compatibility with non-single address wallets
    cbrUsedAddrs :: [GYAddress],
    cbrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)

instance FromJSON CreateBridgeRequest where
  parseJSON = withObject "CreateBridgeRequest" $ \v -> do
    cbrAmount <- v .:? "amount"
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    cbrCollateral <- v .:? "collateral"
    let cbrChangeAddr = unsafeAddressFromText changeAddr
        cbrUsedAddrs = fmap unsafeAddressFromText usedAddrs
    pure CreateBridgeRequest {..}

instance ToJSON CreateBridgeRequest where
  toJSON CreateBridgeRequest {..} =
    object $
      ( case cbrAmount of
          Just am -> ["amount" .= am]
          Nothing -> []
      )
        <> [ "changeAddr" .= addressToText cbrChangeAddr,
             "usedAddrs" .= fmap addressToText cbrUsedAddrs,
             "collateral" .= cbrCollateral
           ]

-- instance FromJSON CreateBridgeRequest where
--   parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "cbr"}

data OfferBountyRequest = OfferBountyRequest
  { obrTopicId :: TopicId,
    obrMessageHash :: Hash,
    obrDeadline :: POSIXTime,
    obrChangeAddr :: GYAddress,
    obrUsedAddrs :: [GYAddress],
    obrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)

instance FromJSON OfferBountyRequest where
  parseJSON = withObject "OfferBountyRequest" $ \v -> do
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    obrCollateral <- v .:? "collateral"
    obrTopicId <- v .: "topicId"
    obrDeadline <- v .: "deadline"
    obrMessageHash <- v .: "messageHash"
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
        "collateral" .= obrCollateral
      ]

-- TODO: save contract addresses and hashes in some cache, to get rid of
-- unnecessary request parameters
-- (why claimant must know about the deadline?)
data ClaimBountyRequest = ClaimBountyRequest
  { cbrTopicId :: TopicId,
    cbrMessageHash :: Hash,
    cbrDeadline :: POSIXTime,
    cBountyrChangeAddr :: GYAddress,
    cBountyrUsedAddrs :: [GYAddress],
    cBountyrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic)

instance FromJSON ClaimBountyRequest where
  parseJSON = withObject "ClaimBountyRequest" $ \v -> do
    changeAddr <- v .: "changeAddr"
    usedAddrs <- v .: "usedAddrs"
    cBountyrCollateral <- v .:? "collateral"
    cbrTopicId <- v .: "topicId"
    cbrDeadline <- v .: "deadline"
    cbrMessageHash <- v .: "messageHash"
    let cBountyrChangeAddr = unsafeAddressFromText changeAddr
        cBountyrUsedAddrs = fmap unsafeAddressFromText usedAddrs
    pure ClaimBountyRequest {..}

instance ToJSON ClaimBountyRequest where
  toJSON ClaimBountyRequest {..} =
    object
      [ "topicId" .= cbrTopicId,
        "messageHash" .= cbrMessageHash,
        "deadline" .= cbrDeadline,
        "changeAddr" .= addressToText cBountyrChangeAddr,
        "usedAddrs" .= fmap addressToText cBountyrUsedAddrs,
        "collateral" .= cBountyrCollateral
      ]
