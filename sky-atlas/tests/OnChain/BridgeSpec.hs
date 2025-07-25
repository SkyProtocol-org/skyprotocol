{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.BridgeSpec (bridgeSpec) where

import App.Env
import Common
import Contract.SkyBridge (bridgeTypedValidatorCore)
import Data.Fixed
import Data.Functor.Identity (Identity (..))
import Data.Time
import Data.Time.Clock qualified as DTC
import Data.Time.Clock.POSIX qualified as DTCP
import GeniusYield.Types
import PlutusLedgerApi.V1.Time qualified as T (POSIXTime (..))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

utcTimeEpoch :: DTC.UTCTime
utcTimeEpoch = DTCP.posixSecondsToUTCTime 0

-- Get current time in Plutus-friendly format
currentPOSIXTime :: IO T.POSIXTime
currentPOSIXTime = do
  utcTime <- getCurrentTime
  let (MkFixed nominalDiffTime) = nominalDiffTimeToSeconds $ diffUTCTime utcTime utcTimeEpoch
  return . T.POSIXTime $ nominalDiffTime `div` 1000000000

-- Create test DA structure and proof
createTestDa :: IO (SkyDa (HashRef Hash), Hash, MultiSigPubKey, CardanoUser)
createTestDa = do
  -- TODO: either hardcode this, or generate some keys for the tests and commit them
  eitherAdmin <- getCardanoUser "tests/OffChain/admin/"
  admin <- case eitherAdmin of
    Left _e -> assertFailure "Can't parse cardano user"
    Right u -> pure u
  let schema = computeDigest (ofHex "deadbeef" :: Bytes4)
      adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey admin
      adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
      committee = MultiSigPubKey ([adminPubKey], UInt16 1)
      da = runIdentity $ initDa schema committee :: SkyDa (HashRef Hash)
  pure (da, schema, committee, admin)

bridgeSpec :: TestTree
bridgeSpec =
  testGroup
    "Bridge Contract"
    [ testGroup
        "Bridge validator"
        [ testCase "multiSigValid" $ do
            (da', _schema, committee, admin) <- createTestDa
            timestamp <- currentPOSIXTime

            let adminSecKeyBytes = let (AGYPaymentSigningKey sk) = cuserSigningKey admin in signingKeyToRawBytes sk
                adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
                adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey admin
                adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
                (da'', maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da'
                topicId = fromJust maybeTopicId
                (da, _maybeMessageId) = runIdentity $ insertMessage timestamp "test message" topicId da''
                topHash = computeDigest @Blake2b_256 da
                signature = signMessage adminSecKey topHash
                bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
            multiSigValid committee topHash bridgeSig @?= True,
          testCase "bridgeTypedValidatorCore" $ do
            (da', schema, committee, admin) <- createTestDa
            timestamp <- currentPOSIXTime

            let adminSecKeyBytes = let (AGYPaymentSigningKey sk) = cuserSigningKey admin in signingKeyToRawBytes sk
                adminSecKey = fromByteString $ BuiltinByteString adminSecKeyBytes
                adminPubKeyBytes = paymentVerificationKeyRawBytes $ cuserVerificationKey admin
                adminPubKey = fromByteString $ BuiltinByteString adminPubKeyBytes
                oldTopHash = computeDigest @Blake2b_256 da'
                (da'', maybeTopicId) = runIdentity $ insertTopic (computeDigest (ofHex "1ea7f00d" :: Bytes4)) da'
                topicId = fromJust maybeTopicId
                (da, _maybeMessageId) = runIdentity $ insertMessage timestamp "test message" topicId da''

            daData <- unwrap $ skyTopicTrie da'

            let topHash = computeDigest @Blake2b_256 da
                signature = signMessage adminSecKey topHash
                bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
                daDataHash = computeDigest @Blake2b_256 daData

            -- bridgeTypedValidatorCore :: Hash -> MultiSigPubKey -> Hash -> Hash -> MultiSig -> Hash -> Bool
            -- bridgeTypedValidatorCore daSchema daCommittee daData newTopHash sig oldTopHash =
            bridgeTypedValidatorCore schema committee daDataHash topHash bridgeSig oldTopHash @?= True
        ]
    ]
