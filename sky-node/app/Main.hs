{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (initApp)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Log.Backend.StandardOutput

main :: IO ()
main = do
  config <- loadYamlSettingsArgs ["config/default.yaml"] useEnv
  withStdOutLogger $ \stdoutLogger -> do
    initApp config stdoutLogger
