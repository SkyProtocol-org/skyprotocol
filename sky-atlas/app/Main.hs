module Main where

import API (AppEnv (..), startApp)
import Common
import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.IORef (newIORef)
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import Log
import Log.Backend.StandardOutput

main :: IO ()
main = do
  config <- loadYamlSettings ["config/local.yaml"] [] requireEnv
  let daSchema = computeHash (ofHex "deadbeef" :: Bytes4)

      sk1 = ofHex "A77CD8BAC4C9ED1134D958827FD358AC4D8346BD589FAB3102117284746FB45E"
      sk2 = ofHex "B2CB983D9764E7CC7C486BEBDBF1C2AA726EF78BB8BC1C97E5139AE58165A00F"
      pk1 = derivePubKey sk1
      pk2 = derivePubKey sk2
      committee = MultiSigPubKey ([pk1, pk2], UInt16 2)

      daData = runIdentity $ initDa daSchema committee :: SkyDa HashRef

  dataRef <- newIORef daData

  withStdOutLogger $ \logger -> do
    runLogT "main" logger defaultLogLevel $ do
      logInfo_ "Initialized logger"
      let appEnv = AppEnv {appConfig = config, daData = dataRef, logger = logger}
      logInfo_ "Starting server"
      liftIO $ startApp appEnv
