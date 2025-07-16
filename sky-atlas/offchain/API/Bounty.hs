module API.Bounty (BountyAPI, bountyServer) where

import API.Bounty.Contracts
import API.SkyMintingPolicy
import API.Types
import App
import Contract.Bounty
import Control.Monad.Reader
import Data.Text (pack)
import GeniusYield.Types
import Log
import PlutusLedgerApi.V1 (POSIXTime (..), ScriptHash (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import Servant

type BountyAPI =
  "bounty"
    :> ( "offer" :> ReqBody '[JSON] OfferBountyRequest :> Post '[JSON] GYTxId
           :<|> "claim" :> ReqBody '[JSON] ClaimBountyRequest :> Post '[JSON] ()
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

      logTrace_ "Constructing body for bounty offering"
      body <-
        runBuilder
          obrUsedAddrs
          obrChangeAddr
          obrCollateral
          $ mkSendSkeleton validatorAddr 10_000_000 GYLovelace (pubKeyHash $ cuserVerificationKey appOfferer)

      tid <- runGY (cuserSigningKey appOfferer) Nothing obrUsedAddrs obrChangeAddr obrCollateral $ pure body
      logTrace_ $ "Transaction id: " <> pack (show tid)
      pure tid

    claimBountyH = undefined
