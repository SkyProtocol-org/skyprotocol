module API.Types where

import Common
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.IORef (IORef)
import GHC.Generics (Generic)
import Log
import Servant

data APIError = APIError String deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppConfig = AppConfig
  { configPort :: Int,
    configLogLevel :: Maybe String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppEnv = AppEnv
  { appConfig :: AppConfig,
    daData :: IORef (SkyDa HashRef),
    logger :: Logger
  }

type AppM = ReaderT AppEnv (LogT (ExceptT APIError Handler))
