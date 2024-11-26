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
import Text.Hex (Text, ByteString, decodeHex, encodeHex)
import PlutusTx.Builtins (toBuiltin, fromBuiltin, BuiltinByteString)
import PlutusLedgerApi.V2 (CurrencySymbol(..))
import Data.Text (pack, unpack)

hexStringToBuiltinByteString :: Text -> Maybe BuiltinByteString
hexStringToBuiltinByteString s = toBuiltin <$> decodeHex s

builtinByteStringToHexString :: BuiltinByteString -> Text
builtinByteStringToHexString bs = encodeHex (fromBuiltin bs)

-- Compute the top hash from a textual hex public key and left and right hashes
computeTopHashWrapper :: Text -> Text -> Text -> Text
computeTopHashWrapper pubKey left right =
  builtinByteStringToHexString th
  where
    (DataHash th) = topHash proofHash pkHash
    pkHash = multiSigToDataHash (MultiSigPubKey [(PubKey (fromJust (hexStringToBuiltinByteString pubKey)))] 1)
    proofHash = merkleProofToDataHash (SimplifiedMerkleProof
                                       (DataHash (fromJust (hexStringToBuiltinByteString left)))
                                       (DataHash (fromJust (hexStringToBuiltinByteString right))))

-- Computes top hash from public key and left and right hashes
main :: IO ()
main =
  getArgs >>= \case
    [pubKey, left, right] -> putStrLn $ (unpack (computeTopHashWrapper (pack pubKey) (pack left) (pack right)))
    args -> putStrLn $ show (merkleProofNFTHashValid (DataHash (fromJust (hexStringToBuiltinByteString "1d3ff551b82c53714e4ce0f9cec89ab4ef8aa92c6da742462f295df9d765814e"))) (DataHash (fromJust (hexStringToBuiltinByteString "1111"))) (DataHash (fromJust (hexStringToBuiltinByteString "4b4f90f0670c7d8d26949bfc1b90de7e12a572094ec1cdf23fec3e1f9a4bcf71"))) (SimplifiedMerkleProof (DataHash (fromJust (hexStringToBuiltinByteString "0000"))) (DataHash (fromJust (hexStringToBuiltinByteString "1111")))))
