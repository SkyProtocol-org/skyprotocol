module App
  ( AppM (..),
    AppConfig (..),
    BlockState (..),
    AppState (..),
    AppEnv (..),
    authCheck,
    nt,
    runApp,
    runQuery,
    runBuilder,
    runGY,
    buildAndRunGY,
    runAnyGY,
    module App.Env,
    module App.Error,
  )
where

import API.Types
import App.Env
import App.Error
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import GeniusYield.GYConfig
import GeniusYield.TxBuilder hiding (User)
import GeniusYield.Types
import Log
import Servant

newtype AppM a = AppM {runAppM :: ReaderT AppEnv (LogT (ExceptT AppError IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader AppEnv,
      MonadError AppError,
      MonadLog,
      MonadIO
    )

runApp :: AppEnv -> AppM a -> IO (Either AppError a)
runApp env (AppM m) = runExceptT $ runLogT "sky-api" (logger env) (fromMaybe Log.LogTrace $ configLogLevel $ appConfig env) $ runReaderT m env

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck $ \(BasicAuthData username password) ->
  if username == "skyAdmin" && password == "1234"
    then return $ Authorized $ User (username <> "@skyprotocol.org")
    else return Unauthorized

nt :: AppEnv -> AppM a -> Handler a
nt env m = do
  eitherRes <- liftIO $ runApp env m
  case eitherRes of
    Right res -> pure res
    Left err -> throwError err500 {errBody = "Something went wrong. Please contact support with this information: " <> appErrorToUserError err}

-- instance MonadRandom AppM => GYTxBuilderMonad AppM where

runQuery :: GYTxQueryMonadIO a -> AppM a
runQuery q = do
  AppEnv {..} <- ask
  let nid = cfgNetworkId $ configAtlas appConfig
  liftIO $ runGYTxQueryMonadIO nid appProviders q

-- TODO replace these with specific wrapped functions instead of generalized `run*`
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
  liftIO $ do
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

buildAndRunGY ::
  GYSomePaymentSigningKey ->
  Maybe GYSomeStakeSigningKey ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadIO (GYTxSkeleton v) ->
  AppM GYTxId
buildAndRunGY psk ssk addrs addr collateral skeleton = do
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
      (skeleton >>= buildTxBody >>= signAndSubmitConfirmed)

runAnyGY ::
  GYSomePaymentSigningKey ->
  Maybe GYSomeStakeSigningKey ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadIO a ->
  AppM a
runAnyGY psk ssk addrs addr collateral action = do
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
      action
