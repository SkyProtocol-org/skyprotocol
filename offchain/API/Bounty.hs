module API.Bounty (BountyApi (..), bountyServer) where

import API.Bounty.Contracts
import API.SkyMintingPolicy
import API.Types
import App
import Common as C
import Contract.Bounty
import Contract.Bridge
import Contract.DaH
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Reader
import Data.Text (pack)
import GHC.Generics (Generic)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import PlutusLedgerApi.V1 (POSIXTime (..), ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Servant
import Servant.Server.Generic

-- TODO: better descriptions
data BountyApi mode = BountyApi
  { offer ::
      mode
        :- Description "Offer bounty"
          :> "offer"
          :> ReqBody '[JSON] OfferBountyRequest
          :> Post '[JSON] GYTxId,
    claim ::
      mode
        :- Description "Claim bounty"
          :> "claim"
          :> ReqBody '[JSON] ClaimBountyRequest
          :> Post '[JSON] ()
  }
  deriving stock (Generic)

bountyServer :: BountyApi (AsServerT AppM)
bountyServer =
  BountyApi
    { offer = offerBountyH,
      claim = claimBountyH
    }
  where
    offerBountyH OfferBountyRequest {..} = do
      AppEnv {..} <- ask

      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appAdmin
          bountyNFTCurrencySymbol = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
          bountyClaimantPubKeyHash = pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appClaimant
          bountyOffererPubKeyHash = pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appOfferer

      currentSlot <- runQuery slotOfCurrentBlock
      let deadlineSlot = unsafeSlotFromInteger $ slotToInteger currentSlot + obrDeadline
      gyDeadline <- runQuery $ slotToEndTime deadlineSlot

      let bountyDeadline = timeToPlutus gyDeadline
          clientParams =
            ClientParams
              { bountyTopicId = obrTopicId,
                bountyMessageHash = obrMessageHash,
                ..
              }
      validatorAddr <- runQuery $ do
        bountyValidatorAddress clientParams

      -- NOTE: we need collateral here, so if user haven't provided one - we create one ourselves
      collateral <- case obrCollateral of
        Nothing -> do
          logTrace_ "Creating utxos for collateral"
          utxos' <- runQuery $ utxosAtAddress (cuserAddress appOfferer) Nothing
          let utxos = utxosToList utxos'
          case utxos of
            (utxo : _) -> pure $ Just $ GYTxOutRefCbor (utxoRef utxo)
            _ -> throwError $ APIError "Can't find utxo for collateral"
        Just c -> pure $ Just c

      logTrace_ "Constructing body for bounty offering"
      body <-
        runBuilder
          obrUsedAddrs
          obrChangeAddr
          collateral
          $ mkSendSkeleton validatorAddr 10_000_000 GYLovelace (cuserAddressPubKeyHash appOfferer)

      tid <- runGY (cuserSigningKey appOfferer) Nothing obrUsedAddrs obrChangeAddr collateral $ pure body
      logTrace_ $ "Transaction id: " <> pack (show tid)
      pure tid

    claimBountyH ClaimBountyRequest {..} = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appAdmin
          skyPolicyId = mintingPolicyId skyPolicy
          skyToken = GYToken skyPolicyId $ configTokenName appConfig
          curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy
          bountyClaimantPubKeyHash = pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appClaimant
          bountyOffererPubKeyHash = pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appOfferer

      currentSlot <- runQuery slotOfCurrentBlock
      let deadlineSlot = unsafeSlotFromInteger $ slotToInteger currentSlot + cbrDeadline
      gyDeadline <- runQuery $ slotToEndTime deadlineSlot

      let bountyDeadline = timeToPlutus gyDeadline
          clientParams =
            ClientParams
              { bountyTopicId = cbrTopicId,
                bountyMessageHash = cbrMessageHash,
                bountyNFTCurrencySymbol = curSym,
                ..
              }
      bridgeUtxos <- runQuery $ do
        addr <- bridgeValidatorAddress $ BridgeParams curSym
        utxosAtAddressWithDatums addr $ Just skyToken
      bountyUtxos <- runQuery $ do
        addr <- bountyValidatorAddress clientParams
        utxosAtAddress addr Nothing

      let utxoWithDatum = flip filter bridgeUtxos $ \(out, _) ->
            let assets = valueToList $ utxoValue out
             in flip any assets $ \case
                  (GYToken pId name, _) -> name == configTokenName appConfig && pId == skyPolicyId
                  _ -> False
      nftRef <- case utxoWithDatum of
        [(utxo, Just _)] -> pure $ utxoRef utxo
        _ -> throwError $ APIError "Can't find bridge nft utxos"

      -- TODO: make this safe
      -- get the first utxo. TODO: search for the one we need
      let bountyUtxo = head $ utxosToList bountyUtxos
          bountyAmount =
            snd
              . head -- get first asset. TODO: search for the one we need
              . valueToList
              $ utxoValue bountyUtxo

      -- TODO: Fix this
      state <- liftIO $ readMVar appStateR
      let da = view (bridgeState . bridgedSkyDa) state
          maybeRmdProof = runIdentity $ getSkyDataProofH (cbrTopicId, cbrMessageId) da :: Maybe (Hash, SkyDataProofH)
      redeemer <- case maybeRmdProof of
        Just (_, proof) -> pure $ ClaimBounty proof
        Nothing -> throwError $ APIError "Can't construct a proof"
      body <-
        runBuilder
          cBountyrUsedAddrs
          cBountyrChangeAddr
          cBountyrCollateral
          $ mkClaimBountySkeleton
            deadlineSlot -- TODO: this should be deadline slot
            (utxoRef bountyUtxo) -- TODO: this should be bounty utxo ref
            (bountyValidator' clientParams)
            nftRef
            redeemer
            (cuserAddress appClaimant)
            bountyAmount
            (pubKeyHash $ cuserVerificationKey appOfferer)
      tid <- runGY (cuserSigningKey appOfferer) Nothing cBountyrUsedAddrs cBountyrChangeAddr cBountyrCollateral $ pure body
      logTrace_ $ "Transaction id: " <> pack (show tid)
      pure ()
