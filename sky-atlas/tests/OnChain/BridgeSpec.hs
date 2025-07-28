{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.BridgeSpec (bridgeSpec) where

import App.Env
import Common
import Contract.Bridge (bridgeTypedValidatorCore)
import Data.Functor.Identity (Identity (..))
import GeniusYield.Types
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Utils
import Prelude

bridgeSpec :: TestTree
bridgeSpec =
  testGroup
    "Bridge Contract"
    [ testGroup
        "Bridge validator"
        [ testCase "multiSigValid" $ do
            admin <- getAdmin "tests/OffChain/admin/"
            let (da', _schema, committee) = createTestDa $ cuserVerificationKey admin
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
            admin <- getAdmin "tests/OffChain/admin/"
            let (da', schema, committee) = createTestDa $ cuserVerificationKey admin
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
