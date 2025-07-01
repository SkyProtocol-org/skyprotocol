{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module API.Bridge (BridgeAPI, bridgeServer) where

import API.SkyMintingPolicy
import API.Types
import Common
import Contract.SkyBridge
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Text
import GHC.Generics
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import PlutusLedgerApi.V1 (ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx.Prelude (BuiltinByteString)
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
    createBridgeH CreateBridgeRequest {..} = do
      AppEnv {..} <- ask
      state <- liftIO $ readMVar appStateR

      let SkyDa {..} = view (blockState . skyDa) state
          topHash = toByteString $ computeDigest @Blake2b_256 $ SkyDa {..}

      body <-
        runBuilder
          cbrUsedAddrs
          cbrChangeAddr
          cbrCollateral
          $ createBridge (configTokenName appConfig) cbrAmount (cuserVerificationKey appAdmin) topHash
      void $ runGY (cuserSigningKey appAdmin) Nothing cbrUsedAddrs cbrChangeAddr cbrCollateral $ pure body

    readBridgeH = throwError $ APIError "Unimplemented"
    updateBridgeH _ = do
      throwError $ APIError "Unimplemented"

createBridge ::
  (HasCallStack, GYTxBuilderMonad m) =>
  GYTokenName ->
  -- | Amount to mint
  Integer ->
  -- | Minting policy signer
  GYPubKeyHash ->
  -- | Top hash
  BuiltinByteString ->
  m (GYTxSkeleton 'PlutusV2)
createBridge tokenName amount mintSigner topHash = do
  let bridgeNFTDatum = BridgeNFTDatum topHash
      skyPolicy = skyMintingPolicy' $ pubKeyHashToPlutus mintSigner
      skyToken = GYToken (mintingPolicyId skyPolicy) tokenName
      -- should be at least 1
      amt = toInteger (max 1 amount)
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy

  bvAddr <- bridgeValidatorAddress $ BridgeParams curSym

  -- skeleton for minting transaction
  let mintSkeleton =
        mustMint @'PlutusV2 (GYBuildPlutusScript $ GYBuildPlutusScriptInlined skyPolicy) unitRedeemer tokenName amt
          <> mustHaveOutput
            ( GYTxOut
                { -- recipient is the bridge validator
                  gyTxOutAddress = bvAddr,
                  gyTxOutValue = valueSingleton skyToken amt,
                  gyTxOutDatum = Just (datumFromPlutusData bridgeNFTDatum, GYTxOutUseInlineDatum),
                  gyTxOutRefS = Nothing
                }
            )
          <> mustBeSignedBy mintSigner
  pure mintSkeleton

updateBridge ::
  (HasCallStack, GYTxMonad m) =>
  m ()
updateBridge = do
  pure ()

data CreateBridgeRequest = CreateBridgeRequest
  { -- | Make it optional, with default 1
    cbrAmount :: Integer,
    -- | wtf is this?
    cbrChangeAddr :: GYAddress,
    -- | compatibility with non-single address wallets
    cbrUsedAddrs :: [GYAddress],
    cbrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic, ToJSON, FromJSON)
