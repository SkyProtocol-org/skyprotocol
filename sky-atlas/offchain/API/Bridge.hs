module API.Bridge (BridgeAPI, bridgeServer) where

import API.Bridge.Contracts
import API.SkyMintingPolicy
import API.Types
import App
import Common
import Contract.SkyBridge
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Data.Foldable (maximumBy)
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeASCII)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import PlutusLedgerApi.Data.V2 (FromData (..), ToData (..))
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant

type BridgeAPI =
  "bridge"
    :> ( "create" :> ReqBody '[JSON] CreateBridgeRequest :> Post '[JSON] GYTxId
           :<|> "read" :> Get '[JSON] Text
           :<|> "update" :> ReqBody '[JSON] UpdateBridgeRequest :> Post '[JSON] GYTxId
       )

bridgeServer :: ServerT BridgeAPI AppM
bridgeServer = createBridgeH :<|> readBridgeH :<|> updateBridgeH
  where
    createBridgeH CreateBridgeRequest {..} = do
      AppEnv {..} <- ask
      state <- liftIO $ readMVar appStateR

      let SkyDa {..} = view (blockState . skyDa) state
          topHash = computeDigest @Blake2b_256 $ SkyDa {..}

      liftIO $ modifyMVar_ appStateW $ \state' -> do
        let newState = set (bridgeState . bridgedSkyDa) SkyDa {..} state'
        modifyMVar_ appStateR . const . return $ newState
        pure newState

      logTrace_ "Building, signing and submitting minting policy"
      tId <-
        buildAndRunGY (cuserSigningKey appAdmin) Nothing cbrUsedAddrs cbrChangeAddr cbrCollateral $
          mkMintingSkeleton (configTokenName appConfig) (cuserAddressPubKey appAdmin) topHash
      logTrace_ $ "Transaction id: " <> pack (show tId)
      pure tId

    readBridgeH = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appAdmin
          skyPolicyId = mintingPolicyId skyPolicy
          skyToken = GYToken skyPolicyId $ configTokenName appConfig
          curSym = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
          bridgeParams = BridgeParams curSym
      utxoWithDatums <- runQuery $ do
        bvAddr <- bridgeValidatorAddress bridgeParams
        -- TODO: what is AssetClass here? Do we need it?
        utxosAtAddressWithDatums bvAddr (Just skyToken)

      let utxoWithDatum = flip filter utxoWithDatums $ \(out, _) ->
            let assets = valueToList $ utxoValue out
             in flip any assets $ \case
                  (GYToken pId name, _) -> name == configTokenName appConfig && pId == skyPolicyId
                  _ -> False
      maybeBridgeDatum <- case utxoWithDatum of
        [(_, Just datum)] -> pure $ fromBuiltinData $ toBuiltinData datum
        _ -> throwError $ APIError "Can't find bridge utxos"
      case maybeBridgeDatum of
        -- will decodeASCII work here? topHash is in hex, if I'm not mistaken
        -- TODO: ask Fare
        Just (BridgeNFTDatum topHash) -> pure . decodeASCII . builtinByteStringToByteString . toByteString $ topHash
        Nothing -> throwError $ APIError "Can't get the datum from bridge"

    updateBridgeH UpdateBridgeRequest {..} = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus $ cuserAddressPubKey appAdmin
          skyPolicyId = mintingPolicyId skyPolicy
          skyToken = GYToken skyPolicyId $ configTokenName appConfig
          curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

      (bridgeAddr, bridgeUtxos) <- runQuery $ do
        addr <- bridgeValidatorAddress $ BridgeParams curSym
        utxos <- utxosAtAddress addr $ Just skyToken
        pure (addr, utxos)

      let utxo = flip filter (utxosToList bridgeUtxos) $ \out ->
            let assets = valueToList $ utxoValue out
             in flip any assets $ \case
                  (GYToken pId name, _) -> name == configTokenName appConfig && pId == skyPolicyId
                  _ -> False
      bridgeUtxo <- case utxo of
        [u] -> pure u
        _ -> throwError $ APIError "Can't find bridge utxos"
      logTrace_ $ "Found bridge utxo: " <> pack (show bridgeUtxo)

      -- NOTE: we need collateral here, so if user haven't provided one - we create one ourselves
      (collateral, _amt) <- do
        mC <- runQuery $ getCollateral' (cuserAddress appAdmin) 10
        case mC of
          Nothing -> throwError $ APIError "Can't find utxo for collateral"
          Just c -> pure c

      -- TODO: what state do we want here? We also need to update it after the successfull update of bridge?
      state <- liftIO $ readMVar appStateR
      let SkyDa {..} = view (blockState . skyDa) state
          topHash = computeDigest @Blake2b_256 $ SkyDa {..}
          newDatum = BridgeNFTDatum topHash
      logTrace_ $ "New utxo datum: " <> pack (show topHash)

      (schema, committee) <- do
        DaMetaDataOfTuple (schema, committee) <- unwrap skyMetaData
        cmt <- unwrap committee
        pure (schema, cmt)

      let bridgedDa = view (bridgeState . bridgedSkyDa) state
          oldRootHash = computeDigest @Blake2b_256 bridgedDa

      let bridgeSchema = schema
          bridgeCommittee = committee
          bridgeOldRootHash = oldRootHash
          bridgeNewTopHash = topHash
          -- TODO: make this safe
          adminSecKeyBytes = let (AGYPaymentSigningKey sk) = cuserSigningKey appAdmin in signingKeyToRawBytes sk
          adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
          signature = signMessage adminSecKey topHash
          adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey appAdmin
          adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
          bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
          bridgeRedeemer = UpdateBridge {..}

      logTrace_ "Building, signing and submitting bridge update"
      tId <-
        buildAndRunGY
          (cuserSigningKey appAdmin)
          Nothing
          ubrUsedAddrs
          ubrChangeAddr
          (if isJust ubrCollateral then ubrCollateral else Just $ GYTxOutRefCbor collateral)
          $ mkUpdateBridgeSkeleton
            (bridgeValidator' $ BridgeParams curSym)
            (utxoRef bridgeUtxo)
            newDatum
            bridgeRedeemer
            skyToken
            bridgeAddr
            (cuserAddressPubKey appAdmin)
      logTrace_ $ "Transaction id: " <> pack (show tId)
      pure tId
