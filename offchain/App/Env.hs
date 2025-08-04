module App.Env where

import App.Error
import Common
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text.IO qualified as T
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GHC.Generics
import GeniusYield.GYConfig
import GeniusYield.Types
import Log
import Log.Backend.StandardOutput
import PlutusLedgerApi.V1 (toBuiltin)
import Utils

-- TODO: add updatable whitelist for users here or in the AppState
data AppEnv = AppEnv
  { appConfig :: AppConfig,
    appProviders :: Maybe GYProviders,
    -- TODO: in the future there must be single node user
    appAdmin :: CardanoUser,
    appClaimant :: CardanoUser,
    appOfferer :: CardanoUser,
    appStateW :: MVar AppState, -- Write copy, lock can be held a long time
    appStateR :: MVar AppState, -- Read copy, lock held very briefly but slightly out-of-date (double buffering / "MVCC")
    logger :: Logger
  }

data AppConfig = AppConfig
  { configPort :: Int,
    configLogLevel :: Maybe LogLevel,
    configTokenName :: GYTokenName,
    configAtlas :: Maybe GYCoreConfig
  }
  deriving (Show, Generic)

instance FromJSON AppConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropPrefix "config"}

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
    cuserAddress :: GYAddress,
    cuserAddressPubKeyHash :: GYPubKeyHash
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
      maybeAddr = addressFromTextMaybe address
      addr = maybe (Left $ StartupError "can't get address") Right maybeAddr
      addrPubKeyHash = maybe (Left $ StartupError "Can't get pubkey from address") Right (maybeAddr >>= addressToPubKeyHash)
  -- putStrLn $ "Initializing cardano user: " <> fp
  -- putStrLn $ "\tverKey: " <> show verKey <> ",\n\tsigKey: " <> show sigKey <> ",\n\taddr: " <> show addr <> ",\n\taddrPubKey: " <> show addrPubKey
  pure $ CardanoUser <$> verKey <*> sigKey <*> addr <*> addrPubKeyHash

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
    _skyDa :: SkyDa (HashRef Hash),
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
  { _bridgedSkyDa :: SkyDa (HashRef Hash) -- data published on the DA *and* bridged on the blockchain
  }

-- TODO: the use of TemplateHaskell is messing with compilers order of definition.
-- need to replace this with generics
$(makeLenses ''BridgeState)
$(makeLenses ''AppState)
$(makeLenses ''BlockState)

initBlockState :: SkyDa (HashRef Hash) -> BlockState
initBlockState da =
  BlockState
    { _skyDa = da,
      _topic = (),
      _erasureCoding = (),
      _superTopic = (),
      _subTopics = (),
      _publisherPayments = ()
    }

initAppState :: BlockState -> BridgeState -> AppState
initAppState blockS bridgeS =
  AppState
    { _blockState = blockS,
      _oldBlockQueue = (),
      _partialSignatures = (),
      _bridgeState = bridgeS,
      _stake = (),
      _peers = (),
      _clients = (),
      _subscriberPayments = (),
      _auctions = (),
      _longTermStorage = ()
    }

initEnv :: AppConfig -> Logger -> Maybe GYProviders -> FilePath -> FilePath -> FilePath -> IO (Either AppError AppEnv)
initEnv appConfig logger appProviders adminKeys offererKeys claimantKeys = do
  eitherAdmin <- getCardanoUser adminKeys
  eitherOfferer <- getCardanoUser offererKeys
  eitherClaimant <- getCardanoUser claimantKeys

  case sequence [eitherAdmin, eitherOfferer, eitherClaimant] of
    Left err -> pure $ Left err
    Right [appAdmin, appOfferer, appClaimant] -> do
      let adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey appAdmin
          adminPubKey = fromByteString $ toBuiltin adminPubKeyBytes
      let daSchema = computeDigest (ofHex "deadbeef" :: Bytes4)
          committee = MultiSigPubKey ([adminPubKey], UInt16 1)
          _skyDa = runIdentity $ initDa daSchema committee :: SkyDa (HashRef Hash)
          _blockState = initBlockState _skyDa
          appState = initAppState _blockState $ BridgeState _skyDa

      appStateW <- newMVar appState
      appStateR <- newMVar appState
      pure $ Right AppEnv {..}
    _ -> pure $ Left $ StartupError "Something went wrong when initializing the environment"

withAppEnv :: FilePath -> FilePath -> FilePath -> (AppEnv -> LogT IO ()) -> IO ()
withAppEnv adminKeys offererKeys claimantKeys f = do
  config <- loadYamlSettings ["config/local-test.yaml"] [] useEnv
  case configAtlas config of
    Nothing -> withStdOutLogger $ \logger -> do
      runLogT "main" logger defaultLogLevel $ do
        logInfo_ "Running in OFFLINE MODE"
        eitherAppEnv <- liftIO $ initEnv config logger Nothing adminKeys offererKeys claimantKeys
        case eitherAppEnv of
          Left err -> liftIO $ print err
          Right appEnv -> f appEnv
    Just atlasConfig ->
      withCfgProviders atlasConfig "api-server" $ \providers -> do
        withStdOutLogger $ \logger -> do
          runLogT "main" logger defaultLogLevel $ do
            logInfo_ "Initialized providers"
            eitherAppEnv <- liftIO $ initEnv config logger (Just providers) adminKeys offererKeys claimantKeys
            case eitherAppEnv of
              Left err -> liftIO $ print err
              Right appEnv -> f appEnv
