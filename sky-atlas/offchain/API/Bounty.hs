module API.Bounty (BountyAPI, bountyServer) where

import API.Bounty.Contracts
import API.SkyMintingPolicy
import API.Types
import App
import Common
import Contract.Bounty
import Contract.SkyBridge
import Control.Monad.Reader
import Data.Text (pack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log
import PlutusLedgerApi.V1 (POSIXTime (..), ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Servant

-- TODO: better descriptions
type BountyAPI =
  Summary "Part of the API to offer and claim a bounty"
    :> "bounty"
    :> ( ( "offer"
             :> Description "Offer bounty"
             :> ReqBody '[JSON] OfferBountyRequest
             :> Post '[JSON] GYTxId
         )
           :<|> "claim"
             :> Description "Claim bounty"
             :> ReqBody '[JSON] ClaimBountyRequest
             :> Post '[JSON] ()
       )

bountyServer :: ServerT BountyAPI AppM
bountyServer = offerBountyH :<|> claimBountyH
  where
    offerBountyH OfferBountyRequest {..} = do
      AppEnv {..} <- ask
      let skyPolicy = skyMintingPolicy' . pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appAdmin
          bountyNFTCurrencySymbol = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
          bountyClaimantPubKeyHash = pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appClaimant
          bountyOffererPubKeyHash = pubKeyHashToPlutus . pubKeyHash $ cuserVerificationKey appOfferer
          bountyDeadline = POSIXTime $ floor $ obrDeadline * 1000
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
          $ mkSendSkeleton validatorAddr 10_000_000 GYLovelace (cuserAddressPubKey appOfferer)

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
          bountyDeadline = POSIXTime $ floor $ cbrDeadline * 1000
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
      let bountyAmount =
            snd
              . head -- get first asset. TODO: search for the one we need
              . valueToList
              . utxoValue
              . head -- get the first utxo. TODO: search for the one we need
              . utxosToList
              $ bountyUtxos

      let proofBytes = toByteString (Byte 1) -- XXX TODO get the real thing
      let redeemer = ClaimBounty proofBytes
      body <-
        runBuilder
          cBountyrUsedAddrs
          cBountyrChangeAddr
          cBountyrCollateral
          $ mkClaimBountySkeleton
            (bountyValidator' clientParams)
            nftRef
            redeemer
            (cuserAddress appClaimant)
            bountyAmount
            (pubKeyHash $ cuserVerificationKey appOfferer)
      tid <- runGY (cuserSigningKey appOfferer) Nothing cBountyrUsedAddrs cBountyrChangeAddr cBountyrCollateral $ pure body
      logTrace_ $ "Transaction id: " <> pack (show tid)
      pure ()
