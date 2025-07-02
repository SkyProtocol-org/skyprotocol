module Main where

import API
import App
import Control.Monad.IO.Class
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GeniusYield.GYConfig (withCfgProviders)
import Log
import Log.Backend.StandardOutput
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import System.Environment
import System.Exit

main :: IO ()
main = do
  -- TODO: replace with normal args handling in the future
  args <- getArgs
  case args of
    ["-h"] -> printHelp
    ["--help"] -> printHelp
    [_, _, _] -> pure ()
    _ -> exitFailure

  -- TODO: temporary for the M4
  let [adminKeys, offererKeys, claimantKeys] = args

  config <- loadYamlSettings ["config/local-test.yaml"] [] useEnv
  withCfgProviders (configAtlas config) "api-server" $ \providers -> do
    withStdOutLogger $ \logger -> do
      runLogT "main" logger defaultLogLevel $ do
        logInfo_ "Initialized logger"
        eitherAppEnv <- liftIO $ initEnv config logger providers adminKeys offererKeys claimantKeys
        case eitherAppEnv of
          Left err -> liftIO $ print err
          Right appEnv -> do
            logInfo_ "Starting server"
            liftIO $
              run (configPort config) $
                cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [HttpTypes.hContentType]}) $
                  app appEnv

printHelp :: IO ()
printHelp = putStrLn "Supply 3 folders paths: admin keys, offerer keys, claimant keys"
