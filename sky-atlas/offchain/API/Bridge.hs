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
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeASCII)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import PlutusLedgerApi.Data.V2 (FromData (..), ToData (..))
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Servant

type BridgeAPI =
  "bridge"
    :> ( "create" :> ReqBody '[JSON] CreateBridgeRequest :> Post '[JSON] ()
           :<|> "read" :> Get '[JSON] Text
           :<|> "update" :> ReqBody '[JSON] Text :> Post '[JSON] Text
       )

bridgeServer :: ServerT BridgeAPI AppM
bridgeServer = createBridgeH :<|> readBridgeH :<|> updateBridgeH
  where
    -- TODO: create a bridged version of the skyda and store it in app state
    createBridgeH CreateBridgeRequest {..} = do
      AppEnv {..} <- ask
      state <- liftIO $ readMVar appStateR

      let SkyDa {..} = view (blockState . skyDa) state
          topHash = toByteString $ computeDigest @Blake2b_256 $ SkyDa {..}

      logTrace_ "Constructing body for the minting policy"
      body <-
        runBuilder cbrUsedAddrs cbrChangeAddr cbrCollateral $
          mkMintingSkeleton cbrAmount (configTokenName appConfig) (pubKeyHash $ cuserVerificationKey appAdmin) topHash

      logTrace_ "Signing and submitting minting policy"
      tId <- runGY (cuserSigningKey appAdmin) Nothing cbrUsedAddrs cbrChangeAddr cbrCollateral $ pure body
      logTrace_ $ "Transaction id: " <> pack (show tId)

    readBridgeH = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appAdmin
          curSym = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
          bridgeParams = BridgeParams curSym
      utxoWithDatums <- runQuery $ do
        bvAddr <- bridgeValidatorAddress bridgeParams
        -- TODO: what is AssetClass here? Do we need it?
        utxosAtAddressWithDatums bvAddr Nothing

      let utxoWithDatum = flip filter utxoWithDatums $ \(out, _) ->
            let assets = valueToList $ utxoValue out
             in flip any assets $ \case
                  (GYToken _ name, _) -> name == configTokenName appConfig
                  _ -> False
      maybeBridgeDatum <- case utxoWithDatum of
        [(_, Just datum)] -> pure $ fromBuiltinData $ toBuiltinData datum
        _ -> throwError $ APIError "Can't find bridge utxos"
      case maybeBridgeDatum of
        -- will decodeASCII work here? topHash is in hex, if I'm not mistaken
        -- TODO: ask Fare
        Just (BridgeNFTDatum topHash) -> pure $ decodeASCII $ builtinByteStringToByteString topHash
        Nothing -> throwError $ APIError "Can't get the datum from bridge"

    updateBridgeH _ = do
      AppEnv {..} <- ask
      throwError $ APIError "Unimplemented"
