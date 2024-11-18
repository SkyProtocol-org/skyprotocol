{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (initApp)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

main :: IO ()
main = do
  logger <- newStdoutLoggerSet defaultBufSize
  config <- loadYamlSettingsArgs ["config/default.yaml"] useEnv
  initApp config logger
