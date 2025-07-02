{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module API.Types where

import Data.Aeson
import Data.ByteString qualified as BS
import GHC.Generics
import GeniusYield.Types

data User = User
  { userEmail :: BS.ByteString
  }

data CreateBridgeRequest = CreateBridgeRequest
  { -- Admin mints tokens, creates bridge
    cbrSigner :: GYPubKeyHash,
    -- | Make it optional, with default 1
    cbrAmount :: Maybe Integer,
    -- | wtf is this?
    cbrChangeAddr :: GYAddress,
    -- | compatibility with non-single address wallets
    cbrUsedAddrs :: [GYAddress],
    cbrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic, ToJSON, FromJSON)
