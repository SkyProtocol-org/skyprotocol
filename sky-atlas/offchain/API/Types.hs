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

instance (Dato a) => PreWrapping AppM HashRef a where
  wrap :: Dato a => a -> AppM (HashRef a)
  wrap = pure . digestRef

instance LiftPreWrapping AppM HashRef where
  liftWrap = wrap

instance (Dato a) => Wrapping AppM HashRef a where
  unwrap :: Dato a => HashRef a -> AppM a
  unwrap DigestRef {..} = pure digestRefValue

instance LiftWrapping AppM HashRef where
  liftUnwrap = unwrap
