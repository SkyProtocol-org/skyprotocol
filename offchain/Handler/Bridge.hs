module Handler.Bridge where

import App
import Common
import Contract.Bridge (BridgeDatum (..), BridgeParams (..), BridgeRedeemer (..))
import Contract.MintingPolicy (SkyMintingParams (..))
import Control.Concurrent.MVar
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeASCII)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log.Class
import PlutusLedgerApi.V1
import Script
import Transaction.Bridge

createBridgeHandler ::
  ( Monad m,
    MonadLog m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m
  ) =>
  m GYTxId
createBridgeHandler = do
  AppEnv {..} <- ask
  let skyPolicy = skyMintingPolicy' . SkyMintingParams . pubKeyHashToPlutus $ cuserAddressPubKeyHash appAdmin
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId $ configTokenName appConfig
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bridgeAddr <- runQuery $ bridgeValidatorAddress $ BridgeParams curSym

  state <- liftIO $ readMVar appStateR
  let SkyDa {..} = state._blockState._skyDa
      topHash = computeDigest @Blake2b_256 $ SkyDa {..}

  liftIO $ modifyMVar_ appStateW $ \state' -> do
    let newBlockState = state'._blockState {_skyDa = SkyDa {..}}
        newState = state' {_blockState = newBlockState}
    modifyMVar_ appStateR . const . return $ newState
    pure newState

  logTrace_ "Building, signing and submitting minting policy"
  tId <-
    buildAndRunGY
      (cuserSigningKey appAdmin)
      Nothing
      [cuserAddress appAdmin]
      (cuserAddress appAdmin)
      Nothing
      $ mkMintingSkeleton
        (configTokenName appConfig)
        skyToken
        skyPolicy
        (BridgeDatum topHash)
        bridgeAddr
        (cuserAddressPubKeyHash appAdmin)
  logTrace_ $ "Transaction id: " <> pack (show tId)
  pure tId

readBridgeHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadIO m
  ) =>
  m Text
readBridgeHandler = do
  AppEnv {..} <- ask
  let skyPolicy = skyMintingPolicy' . SkyMintingParams . pubKeyHashToPlutus $ cuserAddressPubKeyHash appAdmin
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId $ configTokenName appConfig
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy
      bridgeParams = BridgeParams curSym

  utxoWithDatums <- runQuery $ do
    bvAddr <- bridgeValidatorAddress bridgeParams
    utxosAtAddressWithDatums bvAddr (Just skyToken)

  let utxoWithDatum = flip filter utxoWithDatums $ \(out, _) ->
        let assets = valueToList $ utxoValue out
         in flip any assets $ \case
              (GYToken pId name, _) -> name == configTokenName appConfig && pId == skyPolicyId
              _ -> False
  maybeBridgeDatum <- case utxoWithDatum of
    [(_, Just datum)] -> pure $ fromBuiltinData $ datumToPlutus' datum
    _ -> throwError $ APIError "Can't find bridge utxos"
  case maybeBridgeDatum of
    -- will decodeASCII work here? topHash is in hex, if I'm not mistaken
    -- TODO: ask Fare
    Just (BridgeDatum topHash) -> pure . decodeASCII . fromBuiltin . toByteString $ topHash
    Nothing -> throwError $ APIError "Can't get the datum from bridge"

updateBridgeHandler ::
  ( Monad m,
    MonadReader AppEnv m,
    MonadIO m,
    MonadError AppError m,
    MonadLog m
  ) =>
  m GYTxId
updateBridgeHandler = do
  AppEnv {..} <- ask
  let skyPolicy = skyMintingPolicy' . SkyMintingParams . pubKeyHashToPlutus $ cuserAddressPubKeyHash appAdmin
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId $ configTokenName appConfig
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  (bridgeAddr, bridgeUtxos) <- runQuery $ do
    addr <- bridgeValidatorAddress $ BridgeParams curSym
    utxos <- utxosAtAddress addr $ Just skyToken
    pure (addr, utxos)

  logTrace_ $ pack (show bridgeUtxos)
  let utxo = flip filter (utxosToList bridgeUtxos) $ \out ->
        let assets = valueToList $ utxoValue out
         in flip any assets $ \case
              (GYToken pId name, _) -> name == configTokenName appConfig && pId == skyPolicyId
              _ -> False
  bridgeUtxo <- case utxo of
    u : _ -> pure u
    _ -> throwError $ APIError "Can't find bridge utxos"
  logTrace_ $ "Found bridge utxo: " <> pack (show bridgeUtxo)

  -- NOTE: we need collateral here, so if user haven't provided one - we create one ourselves
  (collateral, _amt) <- do
    mC <- runQuery $ getCollateral' (cuserAddress appAdmin) 10
    case mC of
      Nothing -> throwError $ APIError "Can't find utxo for collateral"
      Just c -> pure c

  state <- liftIO $ readMVar appStateR
  let da = state._blockState._skyDa
      topHash = computeDigest @Blake2b_256 $ da
      newDatum = BridgeDatum topHash
  logTrace_ $ "New utxo datum: " <> pack (show topHash)

  (schema, committee) <- do
    DaMetaDataOfTuple (schema, committee) <- unwrap $ skyMetaData da
    cmt <- unwrap committee
    pure (schema, cmt)

  let bridgedDa = state._bridgeState._bridgedSkyDa
  daData <- unwrap $ skyTopicTrie bridgedDa
  let oldRootHash = computeDigest @Blake2b_256 daData

  let bridgeSchema = schema
      bridgeCommittee = committee
      bridgeOldRootHash = oldRootHash
      bridgeNewTopHash = topHash
      -- TODO: make this safe
      adminSecKeyBytes = let (AGYPaymentSigningKey sk) = cuserSigningKey appAdmin in signingKeyToRawBytes sk
      adminSecKey = fromByteString $ toBuiltin adminSecKeyBytes
      signature = signMessage adminSecKey topHash
      adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey appAdmin
      adminPubKey = fromByteString $ toBuiltin adminPubKeyBytes
      bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
      bridgeRedeemer = UpdateBridge {..}

  logTrace_ "Building, signing and submitting bridge update"
  tId <-
    buildAndRunGY
      (cuserSigningKey appAdmin)
      Nothing
      [cuserAddress appAdmin]
      (cuserAddress appAdmin)
      (Just $ GYTxOutRefCbor collateral)
      $ mkUpdateBridgeSkeleton
        (bridgeValidator' $ BridgeParams curSym)
        (utxoRef bridgeUtxo)
        newDatum
        bridgeRedeemer
        skyToken
        bridgeAddr
        (cuserAddressPubKeyHash appAdmin)
  logTrace_ $ "Transaction id: " <> pack (show tId)

  -- update the bridged state after we update the bridge
  liftIO $ modifyMVar_ appStateW $ \state' -> do
    let newBridgeState = state'._bridgeState {_bridgedSkyDa = da}
        newState = state' {_bridgeState = newBridgeState}
    modifyMVar_ appStateR . const . return $ newState
    pure newState

  pure tId
