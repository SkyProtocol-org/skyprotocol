module Util where

import App.Env
import Common
import Data.Functor.Identity (Identity (..))
import GeniusYield.Types
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty.HUnit

-- Create test DA structure and proof
createTestDa :: GYPaymentVerificationKey -> (SkyDa (HashRef Blake2b_256), Blake2b_256, MultiSigPubKey)
createTestDa pubKey =
  let schema = computeDigest (ofHex "deadbeef" :: Bytes4)
      adminPubKeyBytes = paymentVerificationKeyRawBytes pubKey
      adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
      committee = MultiSigPubKey ([adminPubKey], UInt16 1)
      da = runIdentity $ initDa schema committee :: SkyDa (HashRef Blake2b_256)
   in (da, schema, committee)

getAdmin :: FilePath -> IO CardanoUser
getAdmin path = do
  eitherAdmin <- getCardanoUser path
  case eitherAdmin of
    Left _e -> assertFailure "Can't parse cardano user"
    Right u -> pure u
