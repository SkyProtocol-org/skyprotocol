{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import SkyBridgeContract
import BountyContract
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Text.Hex (Text, ByteString, decodeHex)
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusLedgerApi.V2 (CurrencySymbol(..))
import Data.Text (pack)

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

clientParams :: CurrencySymbol -> TopicID -> DataHash -> ClientParams
clientParams csym topicID targetHash =
  ClientParams
    { bountyNFTCurrencySymbol = csym
    , bountyTopicID = topicID
    , bountyMessageHash = targetHash
    }

clientContractBlueprint :: CurrencySymbol -> TopicID -> DataHash -> ContractBlueprint
clientContractBlueprint csym topicID targetHash =
  MkContractBlueprint
    { contractId = Just "client-validator"
    , contractPreamble = clientPreamble
    , contractValidators = Set.singleton (myClientValidator csym topicID targetHash)
    , contractDefinitions = deriveDefinitions @[ClientParams, ClientRedeemer]
    }

clientPreamble :: Preamble
clientPreamble =
  MkPreamble
    { preambleTitle = "Client Validator"
    , preambleDescription = Just "Blueprint for a Plutus script validating auction transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

myClientValidator :: CurrencySymbol -> TopicID -> DataHash -> ValidatorBlueprint referencedTypes
myClientValidator csym topicID targetHash =
  MkValidatorBlueprint
    { validatorTitle = "Client Validator"
    , validatorDescription = Just "Plutus script validating auction transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @ClientParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the auction validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @ClientRedeemer
          }
    , validatorDatum = Nothing
    , validatorCompiled = do
        let script = clientValidatorScript (clientParams csym topicID targetHash)
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: CurrencySymbol -> TopicID -> DataHash -> FilePath -> IO ()
writeBlueprintToFile csym topicID targetHash path = writeBlueprint path (clientContractBlueprint csym topicID targetHash)

main :: IO ()
main =
  getArgs >>= \case
    [csym, topicID, targetHash, path] -> writeBlueprintToFile (CurrencySymbol (fromJust (hexStringToBuiltinByteString (pack csym)))) (TopicID (fromJust (hexStringToBuiltinByteString (pack topicID)))) (DataHash (fromJust (hexStringToBuiltinByteString (pack targetHash)))) path
    args -> fail $ "Expects 3 argument, currency symbol, target hash and path, got " <> show (length args)
