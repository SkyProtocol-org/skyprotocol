{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

-- | Node configuration
data AppConfig = AppConfig
  { port :: String
  }
  deriving (Show, Generic, FromJSON)
