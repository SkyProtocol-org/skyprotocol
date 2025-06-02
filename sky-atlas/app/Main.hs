module Main where

import API (testEnv, startApp)
import Control.Monad.IO.Class
import Log
import Log.Backend.StandardOutput

main :: IO ()
main = do
  withStdOutLogger $ \logger -> do
    runLogT "main" logger defaultLogLevel $ do
      logInfo_ "Initialized logger"
      appEnv <- liftIO $ testEnv logger
      logInfo_ "Starting server"
      liftIO $ startApp appEnv
