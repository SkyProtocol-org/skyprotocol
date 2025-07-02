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
  deriving (Show, Generic, ToJSON, FromJSON)
