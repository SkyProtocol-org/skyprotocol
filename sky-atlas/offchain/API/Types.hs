{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
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

import Data.Text (Text)
import GHC.Generics (Generic)
import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import Servant

data AppError
  = APIError String
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppConfig = AppConfig
  { configPort :: Int,
    configLogLevel :: Maybe Text,
    configTokenName :: Text,
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

$(makeLenses ''BlockState)

data BridgeState = BridgeState
  { _bridgedSkyDa :: SkyDa HashRef -- data published on the DA *and* bridged on the blockchain
  }

$(makeLenses ''BridgeState)


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

$(makeLenses ''AppState)

data CardanoUser = CardanoUser
  { cuserVerificationKey :: Text,
    cuserSigningKey :: Text,
    cuserAddress :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

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

-- TODO: consider integration atlas core monads into the monad stack
type AppM = ReaderT AppEnv (LogT (ExceptT AppError Handler))

data User = User
  { userEmail :: BS.ByteString,
    userPubKey :: PubKey
    -- TODO: add information about payments to the blockchain?
  }

runQuery :: GYTxQueryMonadIO a -> AppM a
runQuery q = do
  AppEnv {..} <- ask
  let nid = cfgNetworkId $ configAtlas appConfig
  liftIO $ runGYTxQueryMonadIO nid appProviders q

runBuilder ::
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxBuilderMonadIO (GYTxSkeleton v) ->
  AppM GYTxBody
runBuilder addrs addr collateral skeleton = do
  AppEnv {..} <- ask
  let nid = cfgNetworkId $ configAtlas appConfig
  liftIO $
    runGYTxBuilderMonadIO
      nid
      appProviders
      addrs
      addr
      ( collateral
          >>= ( \c ->
                  Just
                    ( getTxOutRefHex c,
                      True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                    )
              )
      )
      (skeleton >>= buildTxBody)

runGY ::
  GYSomePaymentSigningKey ->
  Maybe GYSomeStakeSigningKey ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadIO GYTxBody ->
  AppM GYTxId
runGY psk ssk addrs addr collateral body = do
  AppEnv {..} <- ask
  let nid = cfgNetworkId $ configAtlas appConfig
  liftIO $
    runGYTxMonadIO
      nid
      appProviders
      psk
      ssk
      addrs
      addr
      ( collateral
          >>= ( \c ->
                  Just
                    ( getTxOutRefHex c,
                      True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                    )
              )
      )
      (body >>= signAndSubmitConfirmed)
