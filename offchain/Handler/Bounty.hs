module Handler.Bounty where

import App
import Common
import Contract.Bounty (BountyDatum (..), BountyParams (..), BountyRedeemer (..))
import Contract.Bridge (BridgeParams (..))
import Contract.DaH
import Contract.MintingPolicy (SkyMintingParams (..))
import Control.Concurrent.MVar
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Text (pack)
import Data.Time.Clock.POSIX qualified as TP
import GeniusYield.TxBuilder
import GeniusYield.Types
import Log.Class
import PlutusLedgerApi.V1
import Script
import Transaction.Bounty
import Utils

offerBountyHandler ::
  ( Monad m,
    MonadLog m,
    MonadError AppError m,
    MonadIO m,
    MonadReader AppEnv m
  ) =>
  TopicId ->
  Hash ->
  TP.POSIXTime ->
  Integer ->
  -- | Bridge admin public key hash
  GYPubKeyHash ->
  -- | Bounty claimant public key hash
  GYPubKeyHash ->
  -- | Bounty offerer
  CardanoUser ->
  m GYTxId
offerBountyHandler topicId messageHash deadline amount bridgeAdminPubKeyHash claimantPubKeyHash bountyOfferer = do
  let skyPolicy = skyMintingPolicy' . SkyMintingParams $ pubKeyHashToPlutus bridgeAdminPubKeyHash
      bountyCurrencySymbol = CurrencySymbol . getScriptHash . scriptHashToPlutus $ scriptHash skyPolicy
      bountyClaimantPubKeyHash = pubKeyHashToPlutus claimantPubKeyHash
      bountyOffererPubKeyHash = pubKeyHashToPlutus $ cuserAddressPubKeyHash bountyOfferer

  let bountyDeadline = posixTimeToPlutusTime deadline
      bountyParams =
        BountyParams
          { bountyTopicId = topicId,
            bountyMessageHash = messageHash,
            ..
          }
      bountyDatum = BountyDatum {..}

  logTrace_ $ "deadline: " <> pack (show bountyDeadline)
  validatorAddr <- runQuery $ do
    bountyValidatorAddress bountyParams

  logTrace_ "Offering bounty"
  buildAndRunGY
    (cuserSigningKey bountyOfferer)
    Nothing
    [cuserAddress bountyOfferer]
    (cuserAddress bountyOfferer)
    Nothing
    $ mkOfferBountySkeleton
      validatorAddr
      bountyDatum
      amount
      GYLovelace
      (cuserAddressPubKeyHash bountyOfferer)

claimBountyHandler ::
  ( Monad m,
    MonadIO m,
    MonadReader AppEnv m,
    MonadError AppError m,
    MonadLog m
  ) =>
  TopicId ->
  MessageId ->
  Hash ->
  -- | Bridge admin public key hash
  GYPubKeyHash ->
  -- | Bounty offerer public key hash
  GYPubKeyHash ->
  -- | Bounty claimant
  CardanoUser ->
  m GYTxId
claimBountyHandler topicId messageId messageHash bridgeAdminPubKeyHash offererPubKeyHash bountyClaimant = do
  AppEnv {..} <- ask
  let skyPolicy = skyMintingPolicy' . SkyMintingParams $ pubKeyHashToPlutus bridgeAdminPubKeyHash
      skyPolicyId = mintingPolicyId skyPolicy
      skyToken = GYToken skyPolicyId $ configTokenName appConfig
      curSym = CurrencySymbol $ getScriptHash $ scriptHashToPlutus $ scriptHash skyPolicy
      bountyClaimantPubKeyHash = pubKeyHashToPlutus $ cuserAddressPubKeyHash bountyClaimant
      bountyOffererPubKeyHash = pubKeyHashToPlutus offererPubKeyHash

  let bountyParams =
        BountyParams
          { bountyTopicId = topicId,
            bountyMessageHash = messageHash,
            bountyCurrencySymbol = curSym,
            ..
          }

  bridgeUtxos <- runQuery $ do
    addr <- bridgeValidatorAddress $ BridgeParams curSym
    utxosAtAddressWithDatums addr $ Just skyToken
  bountyUtxos <- runQuery $ do
    addr <- bountyValidatorAddress bountyParams
    utxosAtAddressWithDatums addr Nothing

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
  let bountyUtxo = head bountyUtxos
      bountyAmount =
        snd
          . head -- get first asset. TODO: search for the one we need
          . valueToList
          . utxoValue
          $ fst bountyUtxo
      maybeDatum = unsafeFromBuiltinData . datumToPlutus' <$> snd bountyUtxo
  deadline <- case maybeDatum of
    Just BountyDatum {..} -> pure bountyDeadline
    Nothing -> throwError $ APIError "Can't find datum with deadline"

  -- TODO: make this safe, check for over/under-flows, etc
  deadlineSlot' <- runQuery $ enclosingSlotFromTime' $ timeFromPlutus deadline
  -- Since the deadline set by the user can be in the interval of the slot
  -- we should make a margin of error here, when converting
  let deadlineSlot = unsafeSlotFromInteger $ slotToInteger deadlineSlot' - 1

  -- TODO: Fix this
  state <- liftIO $ readMVar appStateR
  let da = state._blockState._skyDa
      maybeRmdProof = runIdentity $ getSkyDataProofH (topicId, messageId) da :: Maybe (Hash, SkyDataProofH)
  redeemer <- case maybeRmdProof of
    Just (messageHash', proof) -> do
      logTrace_ $ "Message hash: " <> hexOf messageHash'
      pure $ ClaimBounty proof
    Nothing -> throwError $ APIError "Can't construct a proof"

  logTrace_ "Claiming bounty"
  buildAndRunGY
    (cuserSigningKey bountyClaimant)
    Nothing
    [cuserAddress bountyClaimant]
    (cuserAddress bountyClaimant)
    Nothing
    $ mkClaimBountySkeleton
      deadlineSlot
      (utxoRef $ fst bountyUtxo)
      (bountyValidator' bountyParams)
      nftRef
      redeemer
      (cuserAddress bountyClaimant)
      bountyAmount
      (pubKeyHash $ cuserVerificationKey bountyClaimant)
