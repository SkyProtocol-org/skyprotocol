module Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

-- | Node configuration
data AppConfig = AppConfig
  { port :: Text,
    host :: Text
  }
  deriving (Show, Generic, FromJSON)
