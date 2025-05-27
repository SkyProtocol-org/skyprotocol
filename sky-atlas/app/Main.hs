module Main where

import API (startApp, AppConfig(..))
import System.Log.Base (newLogBase, LogLevel(..), logInfo)
import Log
import Log.Backend.StandardOutput
import Data.Yaml.Config (loadYamlSettings, requireEnv)

main :: IO ()
main = do
    config <- loadYamlSettings ["config/local.yaml"] [] requireEnv
    let daData = undefined -- Replace with actual initialization logic
    withStdoutLogger $ \logger -> do
        runLogT "main" logger defaultLogLevel $ do
            logInfo_ "Initialized logger"
            let appEnv = AppEnv { appConfig = config, daData = daData, logger = logger }
            startApp appEnv
