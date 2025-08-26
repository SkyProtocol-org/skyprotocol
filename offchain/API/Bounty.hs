module API.Bounty (BountyApi (..), bountyServer) where

import API.Types
import App
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
    { offer = offerBountyH,
      claim = claimBountyH
    }
  where
    offerBountyH OfferBountyRequest {..} = do
      offerBountyHandler obrTopicId obrMessageHash obrDeadline obrAmount
    claimBountyH ClaimBountyRequest {..} = do
      claimBountyHandler cbrTopicId cbrMessageId cbrMessageHash
