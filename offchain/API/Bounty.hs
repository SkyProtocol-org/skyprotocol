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
      skeleton <-
        offerBountyHandler
          obrTopicId
          obrMessageHash
          obrDeadline
          obrAmount
          (cuserAddressPubKeyHash appAdmin)
          (cuserAddressPubKeyHash appClaimant)
          appOfferer

      buildAndRunGY
        (cuserSigningKey appOfferer)
        Nothing
        [cuserAddress appOfferer]
        (cuserAddress appOfferer)
        Nothing
        $ pure skeleton

    claimBountyApiHandler ClaimBountyRequest {..} = do
      AppEnv {..} <- ask
      skeleton <-
        claimBountyHandler
          cbrTopicId
          cbrMessageId
          cbrMessageHash
          (cuserAddressPubKeyHash appAdmin)
          (cuserAddressPubKeyHash appOfferer)
          appClaimant

      buildAndRunGY
        (cuserSigningKey appClaimant)
        Nothing
        [cuserAddress appClaimant]
        (cuserAddress appClaimant)
        Nothing
        $ pure skeleton
