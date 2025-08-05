module Util where

import App.Env
import Common
import Control.Monad.Extra (maybeM)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1 (toBuiltin)
import Test.Tasty.HUnit

-- Create test DA structure and proof
createTestDa :: GYPaymentVerificationKey -> (SkyDa (HashRef Hash), Hash, MultiSigPubKey)
createTestDa pubKey =
  let schema = computeDigest (ofHex "deadbeef" :: Bytes4)
      adminPubKeyBytes = paymentVerificationKeyRawBytes pubKey
      adminPubKey = fromByteString $ toBuiltin adminPubKeyBytes
      committee = MultiSigPubKey ([adminPubKey], UInt16 1)
      da = runIdentity $ initDa schema committee :: SkyDa (HashRef Hash)
   in (da, schema, committee)

getCardanoUser :: IO CardanoUser
getCardanoUser = do
  signingKey <- generateSigningKey
  let cuserVerificationKey = getVerificationKey signingKey
      payKeyHash = verificationKeyHash cuserVerificationKey
      cuserAddress = addressFromPaymentKeyHash GYTestnetPreview payKeyHash
      cuserSigningKey = AGYPaymentSigningKey signingKey
      addrPubKeyHash = addressToPubKeyHash cuserAddress
  case addrPubKeyHash of
    Just cuserAddressPubKeyHash -> pure CardanoUser {..}
    Nothing -> assertFailure "Can't generate adress pub key hash"

-- | Test environment 'WalletInfo' among other things provides nine wallets that
-- be used in tests. For convinience we assign some meaningful names to them.
admin :: Wallets -> User
admin = w1 -- Runs some administrative action, e.g. deplys the script

getAddr :: (GYTxUserQueryMonad m) => m GYAddress
getAddr =
  maybeM
    (throwAppError $ someBackendError "No own addresses")
    pure
    $ listToMaybe
      <$> ownAddresses

getUserAddr :: (GYTxUserQueryMonad m) => User -> m GYAddress
getUserAddr user = pure . NE.head $ userAddresses user
