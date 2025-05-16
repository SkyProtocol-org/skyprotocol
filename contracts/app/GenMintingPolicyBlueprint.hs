{-# LANGUAGE GADTs #-}

module Main where

import Data.ByteString.Short qualified as Short
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text (pack)
import PlutusLedgerApi.Common (BuiltinByteString, serialiseCompiledCode, toBuiltin)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusTx.Blueprint
import SkyMintingPolicy
import System.Environment (getArgs)
import Text.Hex (ByteString, Text, decodeHex)
import Prelude

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

myContractBlueprint :: String -> ContractBlueprint
myContractBlueprint admin_pkh =
  MkContractBlueprint
    { contractId = Just "sky-minting-policy",
      contractPreamble = myPreamble,
      contractValidators = Set.singleton (myValidator admin_pkh),
      contractDefinitions = deriveDefinitions @[SkyMintingParams, ()]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Sky Minting Policy",
      preambleDescription = Just "A simple minting policy",
      preambleVersion = "1.0.0",
      preamblePlutusVersion = PlutusV2,
      preambleLicense = Just "MIT"
    }

myValidator :: String -> ValidatorBlueprint referencedTypes
myValidator admin_pkh =
  MkValidatorBlueprint
    { validatorTitle = "Sky Minting Validator",
      validatorDescription = Just "A simple minting validator",
      validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Minting Validator Parameters",
              parameterDescription = Just "Compile-time validator parameters",
              parameterPurpose = Set.singleton Mint,
              parameterSchema = definitionRef @SkyMintingParams
            }
        ],
      validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer for the minting policy",
            argumentDescription = Just "The minting policy does not use a redeemer, hence ()",
            argumentPurpose = Set.fromList [Mint],
            argumentSchema = definitionRef @()
          },
      validatorDatum = Nothing,
      validatorCompiled = do
        let script = skyMintingPolicyScript (PubKeyHash (fromJust (hexStringToBuiltinByteString (pack admin_pkh))))
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: String -> FilePath -> IO ()
writeBlueprintToFile admin_pkh path = writeBlueprint path (myContractBlueprint admin_pkh)

main :: IO ()
main =
  getArgs >>= \case
    [admin_pkh, path] -> writeBlueprintToFile admin_pkh path
    args -> fail $ "Expects 2 arguments, admin_pkh and path, got: " <> show (length args)
