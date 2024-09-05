module Utils (token, evalT, evalWithArgsT, writePlutusScript) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
  Config (Config),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Evaluate (
  evalScript,
 )
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
  CurrencySymbol(..),
  type PubKeyHash,
  type TxOutRef,
  getScriptHash,
 )

import Control.Lens (over)
import Plutarch.Script (Script (Script))
import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import UntypedPlutusCore qualified as UPLC


import Plutarch.Api.V2 (scriptHash)

import Sky qualified (policy)


-- TODO: take a look https://github.com/mlabs-haskell/ply
-- we need an exact PubKeyHash * TxOutRef to determine a topic token
token ::  PubKeyHash -> TxOutRef -> Either Text CurrencySymbol
token key ref = fmap (CurrencySymbol . scriptToBytes) $ compile (Config DoTracing) $ Sky.policy key ref
  where
    -- TODO: check is the same as PlutusLedgerApi.V2.toBuiltin Cardano.Api.SerialiseRaw.serialiseToRawBytes
    -- @Cardano.Api.Script.ScriptHash . Cardano.Api.Script.hashScript
    -- see:
    -- https://github.com/input-output-hk/plutus-pioneer-program/blob/fourth-iteration/code/Utilities/src/Utilities/Conversions.hs#L57
    -- https://github.com/IntersectMBO/plutus/blob/e8c0b9482a90a3a006002607751a412e21d84ed6/plutus-benchmark/marlowe/src/PlutusBenchmark/Marlowe/Scripts/Semantics.hs#L409
    scriptToBytes = getScriptHash . scriptHash

{-
  currencySymbol :: MintingPolicy -> CurrencySymbol
  currencySymbol = CurrencySymbol . BuiltinByteString . Api.serialiseToRawBytes
    . hashScript . policyToScript

  hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
  hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

  -- Serialize minting policy
  policyToScript :: PlutusV2.MintingPolicy -> PlutusScript PlutusScriptV2
  policyToScript = serializableToScript

  serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
  serializableToScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise
-}

-- TODO: add the below
-- printTerm def validator

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

{-| Applys 'Data' to Script -}
applyArguments :: Script -> [PLC.Data] -> Script
applyArguments (Script p) args =
  let termArgs = fmap (PLC.mkConstant ()) args
      applied t = PLC.mkIterApp () t termArgs
   in Script $ over UPLC.progTerm applied p

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let scriptType = "PlutusScriptV2" :: String
          plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
          content = encodePretty plutusJson
      LBS.writeFile filepath content
