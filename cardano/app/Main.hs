module Main (main) where

import Sky qualified

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Utils (writePlutusScript)

main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"
  writePlutusScript "AlwaysSucceeds" "./compiled/AlwaysSucceeds.json" $ Sky.validator "" -- TODO: test
