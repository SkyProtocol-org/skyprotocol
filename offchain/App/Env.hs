module App.Env where

import API.Types (UserDb (..))
import App.Error
import Common
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.Aeson
import Data.Text.IO qualified as T
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GHC.Generics
import GeniusYield.GYConfig
import GeniusYield.Types
import Log
import Log.Backend.StandardOutput
import PlutusLedgerApi.V1 (toBuiltin)
import System.Exit (exitFailure)
import Utils

-- TODO: add updatable whitelist for users here or in the AppState
data AppEnv = AppEnv
  { appConfig :: AppConfig,
    appProviders :: Maybe GYProviders,
    -- TODO: in the future there must be single node user
    appAdmin :: CardanoUser,
    appClaimant :: CardanoUser,
    appOfferer :: CardanoUser,
    appUsers :: TVar UserDb,
    appStateW :: MVar AppState, -- Write copy, lock can be held a long time
    appStateR :: MVar AppState, -- Read copy, lock held very briefly but slightly out-of-date (double buffering / "MVCC")
    logger :: Logger
  }

data AppConfig = AppConfig
  { configPort :: Int,
    configLogLevel :: Maybe LogLevel,
    configUserDbPath :: FilePath,
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
  { blockState :: BlockState, -- block being defined at the moment

    -- | bridge to the upstream blockchain
    bridgeState :: BridgeState,
    -- Not Implemented Yet:

    -- | Queue SignedBlocks -- old blocks to be gradually forgotten per retention policy
    oldBlockQueue :: (),
    -- | table of candidate blocks being signed but not yet fully completed
    partialSignatures :: (),
    -- | stake for upstream proof-of-stake
    stake :: (),
    -- | peers for block consensus
    peers :: (),
    -- | client connections
    clients :: (),
    -- | table of payments that were accepted * are pending from subscribers
    subscriberPayments :: (),
    -- | auctions for blockspace
    auctions :: (),
    longTermStorage :: ()
  }

-- | long term storage of data, if any
data BlockState = BlockState
  { -- | data published on the DA
    skyDa :: SkyDa (HashRef Hash),
    -- | Not Implemented Yet:
    -- | current topic
    topic :: (),
    -- | validation for erasure coding
    erasureCoding :: (),
    -- | super topic under which this topic operates, if any
    superTopic :: (),
    -- | sub-topics that operate under this topic, if any
    subTopics :: (),
    -- | payments accepted from publishers but not yet fulfilled
    publisherPayments :: ()
  }

newtype BridgeState = BridgeState
  { bridgedSkyDa :: SkyDa (HashRef Hash) -- data published on the DA *and* bridged on the blockchain
  }

initBlockState :: SkyDa (HashRef Hash) -> BlockState
initBlockState da =
  BlockState
    { skyDa = da,
      topic = (),
      erasureCoding = (),
      superTopic = (),
      subTopics = (),
      publisherPayments = ()
    }

initAppState :: BlockState -> BridgeState -> AppState
initAppState blockS bridgeS =
  AppState
    { blockState = blockS,
      bridgeState = bridgeS,
      oldBlockQueue = (),
      partialSignatures = (),
      stake = (),
      peers = (),
      clients = (),
      subscriberPayments = (),
      auctions = (),
      longTermStorage = ()
    }

initEnv :: AppConfig -> TVar UserDb -> Logger -> Maybe GYProviders -> FilePath -> FilePath -> FilePath -> IO (Either AppError AppEnv)
initEnv appConfig appUsers logger appProviders adminKeys offererKeys claimantKeys = do
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
          skyDa = runIdentity $ initDa daSchema committee :: SkyDa (HashRef Hash)
          blockState = initBlockState skyDa
          appState = initAppState blockState $ BridgeState skyDa

      appStateW <- newMVar appState
      appStateR <- newMVar appState
      pure $ Right AppEnv {..}
    _ -> pure $ Left $ StartupError "Something went wrong when initializing the environment"

loadUsers :: FilePath -> IO UserDb
loadUsers fp = do
  eitherUserDb <- eitherDecodeFileStrict fp
  case eitherUserDb of
    Right users -> pure $ UserDb users
    Left err -> do
      putStrLn err
      exitFailure

pollFile :: FilePath -> TVar UserDb -> IO ()
pollFile fp dbVar = do
  let loop = do
        users <- loadUsers fp
        atomically $ writeTVar dbVar users
        threadDelay (5 * 1000000) -- reload every 5s
        loop
  loop

withAppEnv :: FilePath -> FilePath -> FilePath -> (AppEnv -> LogT IO ()) -> IO ()
withAppEnv adminKeys offererKeys claimantKeys f = do
  config <- loadYamlSettings ["config/local-test.yaml"] [] useEnv
  appUsers <- newTVarIO =<< loadUsers config.configUserDbPath
  _ <- forkIO $ pollFile config.configUserDbPath appUsers
  case configAtlas config of
    Nothing -> withStdOutLogger $ \logger -> do
      runLogT "main" logger defaultLogLevel $ do
        logInfo_ "Running in OFFLINE MODE"
        eitherAppEnv <- liftIO $ initEnv config appUsers logger Nothing adminKeys offererKeys claimantKeys
        case eitherAppEnv of
          Left err -> liftIO $ print err
          Right appEnv -> f appEnv
    Just atlasConfig ->
      withCfgProviders atlasConfig "api-server" $ \providers -> do
        withStdOutLogger $ \logger -> do
          runLogT "main" logger defaultLogLevel $ do
            logInfo_ "Initialized providers"
            eitherAppEnv <- liftIO $ initEnv config appUsers logger (Just providers) adminKeys offererKeys claimantKeys
            case eitherAppEnv of
              Left err -> liftIO $ print err
              Right appEnv -> f appEnv
