module API.Bounty (BountyApi (..), bountyServer) where

import API.Types
import App
import Control.Monad.Reader
import GHC.Generics (Generic)
import GeniusYield.Types
import Handler.Bounty
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
          :> Post '[JSON] GYTxId
  }
  deriving stock (Generic)

bountyServer :: BountyApi (AsServerT AppM)
bountyServer =
  BountyApi
    { offer = offerBountyApiHandler,
      claim = claimBountyApiHandler
    }
  where
    offerBountyApiHandler OfferBountyRequest {..} = do
      AppEnv {..} <- ask
      offerBountyHandler
        obrTopicId
        obrMessageHash
        obrDeadline
        obrAmount
        (cuserAddressPubKeyHash appAdmin)
        (cuserAddressPubKeyHash appClaimant)
        appOfferer
    claimBountyApiHandler ClaimBountyRequest {..} = do
      AppEnv {..} <- ask
      claimBountyHandler
        cbrTopicId
        cbrMessageId
        cbrMessageHash
        (cuserAddressPubKeyHash appAdmin)
        (cuserAddressPubKeyHash appOfferer)
        appClaimant
