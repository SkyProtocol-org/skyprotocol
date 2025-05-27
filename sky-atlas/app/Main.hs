module Main where

import API (startApp, AppConfig(..))
import Data.Yaml.Config (loadYamlSettings, requireEnv)

main :: IO ()
main = do
    config <- loadYamlSettings ["config/local.yaml"] [] requireEnv
    putStrLn $ "Starting server on port " ++ show (configPort config)
    startApp config
