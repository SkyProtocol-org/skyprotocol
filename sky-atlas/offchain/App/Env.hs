module App.Env where

import Common
import Control.Concurrent.MVar
import Control.Lens
import Data.Aeson
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics
import GeniusYield.GYConfig
import GeniusYield.Types
import Log
import System.Exit (exitFailure)

data AppEnv = AppEnv
  { appConfig :: AppConfig,
    appProviders :: GYProviders,
    appAdmin :: CardanoUser,
    appClaimant :: CardanoUser,
    appOfferer :: CardanoUser,
    appStateW :: MVar AppState, -- Write copy, lock can be held a long time
    appStateR :: MVar AppState, -- Read copy, lock held very briefly but slightly out-of-date (double buffering / "MVCC")
    -- TODO: add updatable whitelist for users here or in the AppState
    logger :: Logger
  }

data AppConfig = AppConfig
  { configPort :: Int,
    configUserSecKey :: SecKey,
    configLogLevel :: Maybe Text,
    -- NOTE: the token name should be base16 encoded(why?)
    configTokenName :: GYTokenName,
    configAtlas :: GYCoreConfig
  }
  deriving (Show, Generic)

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

data CardanoUser = CardanoUser
  { cuserVerificationKey :: GYPubKeyHash,
    cuserSigningKey :: GYSomePaymentSigningKey,
    cuserAddress :: GYAddress
  }
  deriving (Eq, Show, Generic)

-- TODO: make this an Either SomeError CardanoUser instead
-- e.g. make normal error handling
getCardanoUser :: FilePath -> IO CardanoUser
getCardanoUser fp = do
  maybeVerificationKey <- decodeFileStrict $ fp <> "payment.vkey"
  maybeSigningKey <- decodeFileStrict $ fp <> "payment.skey"
  maybeAddress <- decodeFileStrict $ fp <> "payment.addr"
  case (maybeVerificationKey, AGYPaymentSigningKey <$> maybeSigningKey, maybeAddress) of
    (Just cuserVerificationKey, Just cuserSigningKey, Just cuserAddress) -> pure CardanoUser {..}
    _ -> exitFailure

data AppState = AppState
  { _blockState :: BlockState, -- block being defined at the moment
  -- Not Implemented Yet:

    -- | Queue SignedBlocks -- old blocks to be gradually forgotten per retention policy
    _oldBlockQueue :: (),
    -- | table of candidate blocks being signed but not yet fully completed
    _partialSignatures :: (),
    -- | bridge to the upstream blockchain
    _bridgeState :: BridgeState,
    -- | stake for upstream proof-of-stake
    _stake :: (),
    -- | peers for block consensus
    _peers :: (),
    -- | client connections
    _clients :: (),
    -- | table of payments that were accepted * are pending from subscribers
    _subscriberPayments :: (),
    -- | auctions for blockspace
    _auctions :: (),
    _longTermStorage :: ()
  }

-- long term storage of data, if any

data BlockState = BlockState
  { -- | data published on the DA
    _skyDa :: SkyDa HashRef,
    -- | Not Implemented Yet:
    -- | current topic
    _topic :: (),
    -- | validation for erasure coding
    _erasureCoding :: (),
    -- | super topic under which this topic operates, if any
    _superTopic :: (),
    -- | sub-topics that operate under this topic, if any
    _subTopics :: (),
    -- | payments accepted from publishers but not yet fulfilled
    _publisherPayments :: ()
  }

data BridgeState = BridgeState
  { _bridgedSkyDa :: SkyDa HashRef -- data published on the DA *and* bridged on the blockchain
  }

-- TODO: the use of TemplateHaskell is messing with compiler and order of definition.
-- try to replace this with generics
$(makeLenses ''BridgeState)
$(makeLenses ''AppState)
$(makeLenses ''BlockState)

initEnv :: AppConfig -> Logger -> GYProviders -> FilePath -> FilePath -> FilePath -> IO AppEnv
initEnv appConfig logger appProviders adminKeys offererKeys claimantKeys = do
  let daSchema = computeHash (ofHex "deadbeef" :: Bytes4)
      committee = MultiSigPubKey ([undefined, undefined], UInt16 2)
      _skyDa = runIdentity $ initDa daSchema committee :: SkyDa HashRef
      _blockState =
        BlockState
          { _skyDa,
            _topic = (),
            _erasureCoding = (),
            _superTopic = (),
            _subTopics = (),
            _publisherPayments = ()
          }
      appState =
        AppState
          { _blockState,
            _oldBlockQueue = (),
            _partialSignatures = (),
            _bridgeState = BridgeState _skyDa,
            _stake = (),
            _peers = (),
            _clients = (),
            _subscriberPayments = (),
            _auctions = (),
            _longTermStorage = ()
          }
  appStateW <- newMVar appState
  appStateR <- newMVar appState

  appAdmin <- getCardanoUser adminKeys
  appOfferer <- getCardanoUser offererKeys
  appClaimant <- getCardanoUser claimantKeys

  pure $ AppEnv {..}
