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
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (fromJust, fromMaybe)
import Data.Text.Encoding (decodeUtf8)
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

authCheck :: TVar UserDb -> BasicAuthCheck User
authCheck userDbVar = BasicAuthCheck $ \(BasicAuthData username password) -> do
  userDb <- readTVarIO userDbVar
  let userDb' = (\User {..} -> (userName, User {..})) <$> getUsers userDb
  case lookup (decodeUtf8 username) userDb' of
    Just User {..} ->
      if decodeUtf8 password == userPassword
        then pure $ Authorized User {..}
        else pure BadPassword
    Nothing -> pure NoSuchUser

nt :: AppEnv -> AppM a -> Handler a
nt env m = do
  eitherRes <- liftIO $ runApp env m
  case eitherRes of
    Right res -> pure res
    Left err -> throwError err500 {errBody = "Something went wrong. Please contact support with this information: " <> appErrorToUserError err}

runQuery ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadIO m
  ) =>
  GYTxQueryMonadIO a ->
  m a
runQuery q = do
  AppEnv {..} <- ask
  case configAtlas appConfig of
    Nothing -> throwError $ ProviderError "Providers are not initialized"
    Just atlasConfig -> do
      let nid = cfgNetworkId atlasConfig
      -- if atlasConfig is initialized, we can be sure, that providers are too
      liftIO $ runGYTxQueryMonadIO nid (fromJust appProviders) q

-- TODO replace these with specific wrapped functions instead of generalized `run*`
runBuilder ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadIO m
  ) =>
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxBuilderMonadIO (GYTxSkeleton v) ->
  m GYTxBody
runBuilder addrs addr collateral skeleton = do
  AppEnv {..} <- ask
  case configAtlas appConfig of
    Nothing -> throwError $ ProviderError "Providers are not initialized"
    Just atlasConfig -> do
      let nid = cfgNetworkId atlasConfig
      -- if atlasConfig is initialized, we can be sure, that providers are too
      liftIO $
        runGYTxBuilderMonadIO
          nid
          (fromJust appProviders)
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
  ( Monad m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadIO m
  ) =>
  GYSomePaymentSigningKey ->
  Maybe GYSomeStakeSigningKey ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadIO GYTxBody ->
  m GYTxId
runGY psk ssk addrs addr collateral body = do
  AppEnv {..} <- ask
  case configAtlas appConfig of
    Nothing -> throwError $ ProviderError "Providers are not initialized"
    Just atlasConfig -> do
      let nid = cfgNetworkId atlasConfig
      -- if atlasConfig is initialized, we can be sure, that providers are too
      liftIO $ do
        runGYTxMonadIO
          nid
          (fromJust appProviders)
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
  ( Monad m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadIO m
  ) =>
  GYSomePaymentSigningKey ->
  Maybe GYSomeStakeSigningKey ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadIO (GYTxSkeleton v) ->
  m GYTxId
buildAndRunGY psk ssk addrs addr collateral skeleton = do
  AppEnv {..} <- ask
  case configAtlas appConfig of
    Nothing -> throwError $ ProviderError "Providers are not initialized"
    Just atlasConfig -> do
      let nid = cfgNetworkId atlasConfig
      -- if atlasConfig is initialized, we can be sure, that providers are too
      liftIO $
        runGYTxMonadIO
          nid
          (fromJust appProviders)
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
  ( Monad m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadIO m
  ) =>
  GYSomePaymentSigningKey ->
  Maybe GYSomeStakeSigningKey ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxMonadIO a ->
  m a
runAnyGY psk ssk addrs addr collateral action = do
  AppEnv {..} <- ask
  case configAtlas appConfig of
    Nothing -> throwError $ ProviderError "Providers are not initialized"
    Just atlasConfig -> do
      let nid = cfgNetworkId atlasConfig
      -- if atlasConfig is initialized, we can be sure, that providers are too
      liftIO $
        runGYTxMonadIO
          nid
          (fromJust appProviders)
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
