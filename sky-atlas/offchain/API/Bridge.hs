{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{-# LANGUAGE OverloadedStrings #-}

module API.Bridge (BridgeAPI, bridgeServer) where

import API.SkyMintingPolicy
import API.Types
import Common
import Contract.SkyBridge
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Data.Aeson
import Data.Text
import GHC.Generics
import GeniusYield.TxBuilder
import GeniusYield.Types
import Servant
import PlutusLedgerApi.V1 (ScriptHash(..))
import PlutusLedgerApi.V1.Value (CurrencySymbol(..))
import Control.Monad
import GHC.Stack (HasCallStack)
import PlutusTx.Prelude (BuiltinByteString)

data CreateBridgeRequest = CreateBridgeRequest
  { -- | signer is the admin?
    -- Manuel set up three addresses, one for admin, one for offerer and one for claimant
    -- Admin mints tokens, creates bridge
    cbrSigner :: GYPubKeyHash,
    -- | Make it optional, with default 1
    cbrAmount :: Integer,
    -- | wtf is this?
    cbrChangeAddr :: GYAddress,
    -- | compatibility with non-single address wallets
    cbrUsedAddrs :: [GYAddress],
    cbrCollateral :: Maybe GYTxOutRefCbor
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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

      -- TODO this should come from somewhere and be initialized in AppEnv
      psk <- liftIO $ readSomePaymentSigningKey ""

      body <- runBuilder cbrUsedAddrs cbrChangeAddr cbrCollateral $ createBridge cbrAmount cbrSigner topHash
      void $ runGY psk Nothing cbrUsedAddrs cbrChangeAddr cbrCollateral $ pure body

    readBridgeH = throwError $ APIError "Unimplemented"
    updateBridgeH _ = throwError $ APIError "Unimplemented"


createBridge :: (HasCallStack, GYTxBuilderMonad m)
  => -- | Amount to mint
     Integer ->
     -- | Minting policy signer
     GYPubKeyHash ->
     -- | Top hash
     BuiltinByteString ->
     m (GYTxSkeleton 'PlutusV2)
createBridge amount mintSigner topHash = do
      let  -- bridge datum
          bridgeNFTDatum = BridgeNFTDatum topHash

      -- TODO tokenName should be in the config
      let tokenName = "SkyBridge"
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
                    -- recipient is the bridge validator
                    { gyTxOutAddress = bvAddr,
                      gyTxOutValue = valueSingleton skyToken amt,
                      -- out datum is for the bridge validator?
                      -- copied from setup-test.sh & mint-nft.mjs from Manuel
                      -- NOTE: don't know if we want to inline or no and what does that mean,
                      -- but Manuel set `inline` to `true` so here we are
                      gyTxOutDatum = Just (datumFromPlutusData bridgeNFTDatum, GYTxOutUseInlineDatum),
                      gyTxOutRefS = Nothing
                    }
                )
              <> mustBeSignedBy mintSigner
      pure mintSkeleton

updateBridge :: (HasCallStack, GYTxMonad m)
  => m ()
updateBridge = do
  pure ()
