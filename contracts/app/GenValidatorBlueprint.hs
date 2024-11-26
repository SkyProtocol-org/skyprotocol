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

bridgeParams :: CurrencySymbol -> BridgeParams
bridgeParams csym =
  BridgeParams
    { bridgeNFTCurrencySymbol = csym
    }

bridgeContractBlueprint :: CurrencySymbol -> ContractBlueprint
bridgeContractBlueprint csym =
  MkContractBlueprint
    { contractId = Just "bridge-validator"
    , contractPreamble = bridgePreamble
    , contractValidators = Set.singleton (myBridgeValidator csym)
    , contractDefinitions = deriveDefinitions @[BridgeParams, BridgeRedeemer, BridgeNFTDatum]
    }

bridgePreamble :: Preamble
bridgePreamble =
  MkPreamble
    { preambleTitle = "Bridge Validator"
    , preambleDescription = Just "Blueprint for a Plutus script validating auction transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

myBridgeValidator :: CurrencySymbol -> ValidatorBlueprint referencedTypes
myBridgeValidator csym =
  MkValidatorBlueprint
    { validatorTitle = "Bridge Validator"
    , validatorDescription = Just "Plutus script validating auction transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @BridgeParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the auction validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @BridgeRedeemer
          }
    , validatorDatum = Nothing
    , validatorCompiled = do
        let script = bridgeValidatorScript (bridgeParams csym)
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: CurrencySymbol -> FilePath -> IO ()
writeBlueprintToFile csym path = writeBlueprint path (bridgeContractBlueprint csym)

main :: IO ()
main =
  getArgs >>= \case
    [csym, path] -> writeBlueprintToFile (CurrencySymbol (fromJust (hexStringToBuiltinByteString (pack csym)))) path
    args -> fail $ "Expects 2 argument, currency symbol and path, got " <> show (length args)
