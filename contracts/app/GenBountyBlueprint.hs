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

import SkyContracts
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

clientParams :: CurrencySymbol -> DataHash -> ClientParams
clientParams csym targetHash =
  ClientParams
    { bountyNFTCurrencySymbol = csym
    , bountyTargetHash = targetHash
    }

clientContractBlueprint :: CurrencySymbol -> DataHash -> ContractBlueprint
clientContractBlueprint csym targetHash =
  MkContractBlueprint
    { contractId = Just "client-validator"
    , contractPreamble = clientPreamble
    , contractValidators = Set.singleton (myClientValidator csym targetHash)
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

myClientValidator :: CurrencySymbol -> DataHash -> ValidatorBlueprint referencedTypes
myClientValidator csym targetHash =
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
        let script = clientValidatorScript (clientParams csym targetHash)
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: CurrencySymbol -> DataHash -> FilePath -> IO ()
writeBlueprintToFile csym targetHash path = writeBlueprint path (clientContractBlueprint csym targetHash)

main :: IO ()
main =
  getArgs >>= \case
    [csym, targetHash, path] -> writeBlueprintToFile (CurrencySymbol (fromJust (hexStringToBuiltinByteString (pack csym)))) (DataHash (fromJust (hexStringToBuiltinByteString (pack targetHash)))) path
    args -> fail $ "Expects 3 argument, currency symbol, target hash and path, got " <> show (length args)
