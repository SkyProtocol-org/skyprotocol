{-# LANGUAGE PatternSynonyms, ImpredicativeTypes #-}

module Main (main) where

import Data.Foldable (traverse_)

import Sky qualified

import PlutusLedgerApi.V2 (pattern TxOutRef)

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Utils (writePlutusScript, token)


main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"
  let topicRef = TxOutRef "" 0
  writePlutusScript "SkyProtocolPolicy" "./compiled/Topic.json" $ Sky.policy "" topicRef
  writePlutusScript "SkyProtocolBridge" "./compiled/Bridge.json" `traverse_` (Sky.bridge <$> token "" topicRef)
  writePlutusScript "SkyProtocolClient" "./compiled/Client.json" `traverse_` (Sky.client <$> token "" topicRef)
 -- TODO: test
