{-# LANGUAGE GADTs #-}

module Main where

import SkyBridgeContract
import BountyContract
import Data.ByteString.Short qualified as Short
import Data.Maybe (Maybe, fromJust)
import Data.Set qualified as Set
import Data.Text (pack)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1 (PubKeyHash (..))
import PlutusLedgerApi.V2 (CurrencySymbol (..))
import PlutusTx.Blueprint
import PlutusTx.Builtins (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import SkyBridgeContract
import SkyCrypto
import SkyDA (TopicId)
import System.Environment (getArgs)
import Text.Hex (ByteString, Text, decodeHex)
import Prelude

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

clientParams :: CurrencySymbol -> TopicID -> DataHash -> PubKeyHash -> ClientParams
clientParams csym topicID messageHash claimantPKH =
  ClientParams
    { bountyNFTCurrencySymbol = csym
    , bountyTopicID = topicID
    , bountyMessageHash = messageHash
    , bountyClaimantPubKeyHash = claimantPKH
    }

clientContractBlueprint :: CurrencySymbol -> TopicID -> DataHash -> PubKeyHash -> ContractBlueprint
clientContractBlueprint csym topicID messageHash claimantPKH =
  MkContractBlueprint
    { contractId = Just "client-validator",
      contractPreamble = clientPreamble,
      contractValidators = Set.singleton (myClientValidator csym topicID messageHash claimantPKH),
      contractDefinitions = deriveDefinitions @[ClientParams, ClientRedeemer]
    }

clientPreamble :: Preamble
clientPreamble =
  MkPreamble
    { preambleTitle = "Client Validator",
      preambleDescription = Just "Blueprint for a Plutus script validating auction transactions",
      preambleVersion = "1.0.0",
      preamblePlutusVersion = PlutusV2,
      preambleLicense = Just "MIT"
    }

myClientValidator :: CurrencySymbol -> TopicID -> DataHash -> PubKeyHash -> ValidatorBlueprint referencedTypes
myClientValidator csym topicID messageHash claimantPKH =
  MkValidatorBlueprint
    { validatorTitle = "Client Validator",
      validatorDescription = Just "Plutus script validating auction transactions",
      validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters",
              parameterDescription = Just "Compile-time validator parameters",
              parameterPurpose = Set.singleton Spend,
              parameterSchema = definitionRef @ClientParams
            }
        ],
      validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer",
            argumentDescription = Just "Redeemer for the auction validator",
            argumentPurpose = Set.fromList [Spend],
            argumentSchema = definitionRef @ClientRedeemer
          },
      validatorDatum = Nothing,
      validatorCompiled = do
        let script = clientValidatorScript (clientParams csym topicID messageHash claimantPKH)
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: CurrencySymbol -> TopicID -> DataHash -> PubKeyHash -> FilePath -> IO ()
writeBlueprintToFile csym topicID messageHash claimantPKH path =
  writeBlueprint path (clientContractBlueprint csym topicID messageHash claimantPKH)

main :: IO ()
main =
  getArgs >>= \case
    [csym, topicID, messageHash, claimantPKH, path] ->
      writeBlueprintToFile
        (CurrencySymbol (fromJust (hexStringToBuiltinByteString (pack csym))))
        (TopicID (fromJust (hexStringToBuiltinByteString (pack topicID))))
        (DataHash (fromJust (hexStringToBuiltinByteString (pack messageHash))))
        (PubKeyHash (fromJust (hexStringToBuiltinByteString (pack claimantPKH))))
        path
    args -> fail $ "Expects arguments: currency symbol, topic ID, message hash, claimaint pubkeyhash, and path.  Got: " <> show (length args)
