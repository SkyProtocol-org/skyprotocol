module App.Env where

import App.Error
import Common
import Control.Concurrent.MVar
import Control.Lens
import Data.Aeson
import Data.Char (toLower)
import Data.Text.IO qualified as T
import GHC.Generics
import GeniusYield.GYConfig
import GeniusYield.Types
import Log

-- TODO: add updatable whitelist for users here or in the AppState
data AppEnv = AppEnv
  { appConfig :: AppConfig,
    appProviders :: GYProviders,
    appAdmin :: CardanoUser,
    appClaimant :: CardanoUser,
    appOfferer :: CardanoUser,
    appStateW :: MVar AppState, -- Write copy, lock can be held a long time
    appStateR :: MVar AppState, -- Read copy, lock held very briefly but slightly out-of-date (double buffering / "MVCC")
    logger :: Logger
  }

data AppConfig = AppConfig
  { configPort :: Int,
    configUserSecKey :: SecKey,
    configLogLevel :: Maybe LogLevel,
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

-- | Helper type to read the cardano-cli generated verification key
data CardanoVerificationKey = VerKey
  { verKeyType :: String,
    verKeyDescription :: String,
    verKeyCborHex :: GYPaymentVerificationKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardanoVerificationKey where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "verKey"}

-- | Helper type to read the cardano-cli generated signing key
data CardanoSigningKey = SigKey
  { sigKeyType :: String,
    sigKeyDescription :: String,
    sigKeyCborHex :: GYPaymentSigningKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardanoSigningKey where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "sigKey"}

data CardanoUser = CardanoUser
  { cuserVerificationKey :: GYPaymentVerificationKey,
    cuserSigningKey :: GYSomePaymentSigningKey,
    cuserAddress :: GYAddress
  }
  deriving (Eq, Show, Generic)

-- TODO: make this name agnostic e.g. target the extension
getCardanoUser :: FilePath -> IO (Either AppError CardanoUser)
getCardanoUser fp = do
  eitherVerificationKey <- eitherDecodeFileStrict $ fp <> "payment.vkey"
  eitherSigningKey <- eitherDecodeFileStrict $ fp <> "payment.skey"
  address <- T.readFile $ fp <> "payment.addr"
  let toLeft = Left . StartupError . show
      verKey = either toLeft (Right . verKeyCborHex) eitherVerificationKey
      sigKey = either toLeft (Right . AGYPaymentSigningKey . sigKeyCborHex) eitherSigningKey
      addr = maybe (Left $ StartupError "can't get address") Right $ addressFromTextMaybe address
  pure $ CardanoUser <$> verKey <*> sigKey <*> addr

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

-- TODO: the use of TemplateHaskell is messing with compilers order of definition.
-- need to replace this with generics
$(makeLenses ''BridgeState)
$(makeLenses ''AppState)
$(makeLenses ''BlockState)

initEnv :: AppConfig -> Logger -> GYProviders -> FilePath -> FilePath -> FilePath -> IO (Either AppError AppEnv)
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

  eitherAdmin <- getCardanoUser adminKeys
  eitherOfferer <- getCardanoUser offererKeys
  eitherClaimant <- getCardanoUser claimantKeys

  case sequence [eitherAdmin, eitherOfferer, eitherClaimant] of
    Left err -> pure $ Left err
    Right [appAdmin, appOfferer, appClaimant] -> pure $ Right AppEnv {..}
    _ -> pure $ Left $ StartupError "Something wen't wrong when initializing environment"
