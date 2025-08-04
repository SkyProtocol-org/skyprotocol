{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain.BridgeSpec (bridgeSpec) where

import App.Env
import Common
import Contract.Bridge
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Data.Functor.Identity (Identity (..))
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GeniusYield.Types
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude qualified as PlutusTx
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
        [ testCase "to/from builtin data BridgeNFTDatum" $ do
            let datum = BridgeNFTDatum $ ofHex "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
            let builtinDatum = PlutusTx.toBuiltinData datum
            PlutusTx.fromBuiltinData builtinDatum @?= Just datum,
          testCase "multiSigValid" $ do
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
                daDataHash' = refDigest . skyTopicTrie $ da'

            daData <- unwrap $ skyTopicTrie da'

            let topHash = computeDigest @Blake2b_256 da
                signature = signMessage adminSecKey topHash
                bridgeSig = MultiSig [SingleSig (adminPubKey, signature)]
                daDataHash = computeDigest @Blake2b_256 daData

            -- bridgeTypedValidatorCore :: Hash -> MultiSigPubKey -> Hash -> Hash -> MultiSig -> Hash -> Bool
            -- bridgeTypedValidatorCore daSchema daCommittee daData newTopHash sig oldTopHash =
            daDataHash' @?= daDataHash
            bridgeTypedValidatorCore schema committee daDataHash topHash bridgeSig oldTopHash @?= True
        ]
    ]
