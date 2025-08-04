module Util where

import App.Env
import Common
import Data.Functor.Identity (Identity (..))
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
