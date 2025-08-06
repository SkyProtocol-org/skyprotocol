module API.Bridge (BridgeApi (..), bridgeServer) where

import API.Bridge.Contracts
import API.SkyMintingPolicy
import API.Types
import App
import Common
import Contract.Bridge
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeASCII)
import GHC.Generics (Generic)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import PlutusLedgerApi.Common (fromBuiltin, toBuiltin)
import PlutusLedgerApi.Data.V2 (FromData (..))
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Servant
import Servant.Server.Generic

-- TODO: better descriptions
data BridgeApi mode = BridgeApi
  { create ::
      mode
        :- "create"
          :> Description "Mint token and create bridge"
          :> ReqBody '[JSON] CreateBridgeRequest
          :> Post '[JSON] GYTxId,
    read ::
      mode
        :- "read"
          :> Description "Read current datum from bridge"
          :> Get '[JSON] Text,
    update ::
      mode
        :- "update"
          :> Description "Update bridge datum"
          :> ReqBody '[JSON] UpdateBridgeRequest
          :> Post '[JSON] GYTxId
  }
  deriving stock (Generic)

bridgeServer :: BridgeApi (AsServerT AppM)
bridgeServer =
  BridgeApi
    { create = createBridgeH,
      read = readBridgeH,
      update = updateBridgeH
    }
  where
    createBridgeH CreateBridgeRequest {..} = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus $ cuserAddressPubKeyHash appAdmin
          skyPolicyId = mintingPolicyId skyPolicy
          skyToken = GYToken skyPolicyId $ configTokenName appConfig
          curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

      bridgeAddr <- runQuery $ bridgeValidatorAddress $ BridgeParams curSym

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
          mkMintingSkeleton
            (configTokenName appConfig)
            skyToken
            skyPolicy
            topHash
            bridgeAddr
            (cuserAddressPubKeyHash appAdmin)
      logTrace_ $ "Transaction id: " <> pack (show tId)
      pure tId

    -- TODO: consider returning topHash from the bridged version of the da?
    -- TODO: consider comparing to the bridged version?
    readBridgeH = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus $ cuserAddressPubKeyHash appAdmin
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
        Just (BridgeNFTDatum topHash) -> pure . decodeASCII . fromBuiltin . toByteString $ topHash
        Nothing -> throwError $ APIError "Can't get the datum from bridge"

    updateBridgeH UpdateBridgeRequest {..} = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus $ cuserAddressPubKeyHash appAdmin
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
      let da = view (blockState . skyDa) state
          topHash = computeDigest @Blake2b_256 $ da
          newDatum = BridgeNFTDatum topHash
      logTrace_ $ "New utxo datum: " <> pack (show topHash)

      (schema, committee) <- do
        DaMetaDataOfTuple (schema, committee) <- unwrap $ skyMetaData da
        cmt <- unwrap committee
        pure (schema, cmt)

      let bridgedDa = view (bridgeState . bridgedSkyDa) state
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
            (cuserAddressPubKeyHash appAdmin)
      logTrace_ $ "Transaction id: " <> pack (show tId)

      -- update the bridged state after we update the bridge
      liftIO $ modifyMVar_ appStateW $ \state' -> do
        let newState = set (bridgeState . bridgedSkyDa) da state'
        modifyMVar_ appStateR . const . return $ newState
        pure newState

      pure tId
