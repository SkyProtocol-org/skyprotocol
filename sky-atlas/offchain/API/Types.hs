module API.Types where

import Common
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Char (toLower)
-- import Data.Text (Text)
import GHC.Generics (Generic)
import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.Types (GYProviders)
import Log
import Servant

data AppError
  = APIError String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppConfig = AppConfig
  { configPort :: Int,
    configLogLevel :: Maybe String,
    configAtlas :: GYCoreConfig
  }
  deriving (Show, Generic)

-- instance ToJSON AppConfig where
--   toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "config"}

instance FromJSON AppConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "config"}

dropPrefix :: String -> String -> String
dropPrefix pr s = case splitAt (length pr) s of
  (p, rest) | p == pr -> toLowerHead rest
  _ -> s
  where
    toLowerHead :: String -> String
    toLowerHead [] = []
    toLowerHead (x : xs) = toLower x : xs

data BlockState = BlockState
  { _skyDa :: SkyDa HashRef, -- ^ data published on the DA
  -- Not Implemented Yet:
    _topic :: (),
    -- ^ current topic
    _erasureCoding :: (),
    -- ^ validation for erasure coding
    _superTopic :: (),
    -- ^ super topic under which this topic operates, if any
    _subTopics :: (),
    -- ^ sub-topics that operate under this topic, if any
    _publisherPayments :: ()
  }

data BridgeState = BridgeState
  { _bridgedSkyDa :: SkyDa HashRef -- data published on the DA *and* bridged on the blockchain
    }

-- payments accepted from publishers but not yet fulfilled

$(makeLenses ''BlockState)

data AppState = AppState
  { _blockState :: BlockState, -- block being defined at the moment
  -- Not Implemented Yet:
    _oldBlockQueue :: (),
    -- ^ Queue SignedBlocks -- old blocks to be gradually forgotten per retention policy
    _partialSignatures :: (),
    -- ^ table of candidate blocks being signed but not yet fully completed
    _bridgeState :: BridgeState,
    -- ^ bridge to the upstream blockchain
    _stake :: (),
    -- ^ stake for upstream proof-of-stake
    _peers :: (),
    -- ^ peers for block consensus
    _clients :: (),
    -- ^ client connections
    _subscriberPayments :: (),
    -- ^ table of payments that were accepted * are pending from subscribers
    _auctions :: (),
    -- ^ auctions for blockspace
    _longTermStorage :: ()
  }

-- long term storage of data, if any

$(makeLenses ''AppState)

data AppEnv = AppEnv
  { appConfig :: AppConfig,
    appProviders :: GYProviders,
    appStateW :: MVar AppState, -- Write copy, lock can be held a long time
    appStateR :: MVar AppState, -- Read copy, lock held very briefly but slightly out-of-date (double buffering / "MVCC")
    -- TODO: add updatable whitelist for users here or in the AppState
    logger :: Logger
  }

type AppM = ReaderT AppEnv (LogT (ExceptT AppError Handler))

data User = User
  { userEmail :: BS.ByteString,
    userPubKey :: PubKey
    -- TODO: add information about payments to the blockchain?
  }
