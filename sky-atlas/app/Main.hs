module Main where

import API
import Control.Monad.IO.Class
import Data.Yaml.Config (loadYamlSettings, useEnv)
import GeniusYield.GYConfig (withCfgProviders)
import Log
import Log.Backend.StandardOutput
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors

main :: IO ()
main = do
  config <- loadYamlSettings ["config/local-test.yaml"] [] useEnv
  withCfgProviders (configAtlas config) "api-server" $ \providers -> do
    withStdOutLogger $ \logger -> do
      runLogT "main" logger defaultLogLevel $ do
        logInfo_ "Initialized logger"
        appEnv <- liftIO $ testEnv config logger providers
        logInfo_ "Starting server"
        liftIO $
          run (configPort config) $
            cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [HttpTypes.hContentType]}) $
              app appEnv
