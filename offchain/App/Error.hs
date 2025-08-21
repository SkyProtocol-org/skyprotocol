module App.Error
  ( AppError (..),
    appErrorToUserError,
  )
where

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 (pack)
import GHC.Generics (Generic)

data AppError
  = APIError String
  | StartupError String
  | ProviderError String
  | DaError String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Function to convert app error to user error that user can send to support
-- TODO: make a normal description for all of this
appErrorToUserError :: AppError -> BSL.ByteString
appErrorToUserError (APIError msg) = pack msg
appErrorToUserError (StartupError msg) = pack msg
appErrorToUserError (ProviderError msg) = pack msg
appErrorToUserError (DaError msg) = pack msg
