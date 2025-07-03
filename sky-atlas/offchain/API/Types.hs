module API.Types where

import Data.Aeson
import Data.ByteString qualified as BS
import GHC.Generics
import GeniusYield.Types

newtype User = User
  { userEmail :: BS.ByteString
  }

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

-- instance FromJSON CreateBridgeRequest where
--   parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "cbr"}
