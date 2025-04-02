{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (initSocketApp)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Log.Backend.StandardOutput

main :: IO ()
main = do
  config <- loadYamlSettingsArgs ["config/default.yaml"] useEnv
  withStdOutLogger $ \stdoutLogger -> do
    initSocketApp config stdoutLogger
