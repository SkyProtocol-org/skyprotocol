module Main where

import API
import App
import Control.Monad.IO.Class
import Log
import Log.Backend.StandardOutput
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment

main :: IO ()
main = do
  -- TODO: replace with normal args handling in the future
  args <- getArgs
  case args of
    [_, _, _] -> putStrLn "Everything is OK, continuing with initialization"
    _ -> printHelp

  -- TODO: temporary for the M4
  let [adminKeys, offererKeys, claimantKeys] = args

  withAppEnv adminKeys offererKeys claimantKeys $ \appEnv -> do
    logInfo_ "Starting server"
    liftIO $
      run (configPort $ appConfig appEnv) $
        cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = [HttpTypes.hContentType]}) $
          logStdoutDev $
            app appEnv

printHelp :: IO ()
printHelp = putStrLn "Supply 3 folders paths: admin keys, offerer keys, claimant keys"
