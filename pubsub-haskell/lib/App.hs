module App (runApp) where

-- import App.Error
import Config (AppConfig)
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)

-- initAppState :: IO ()
-- initAppState = pure ()

runApp :: AppConfig -> LoggerSet -> IO ()
runApp config logger = do
  -- state <- initAppState
  print config
  pushLogStrLn logger $ toLogStr "Starting Sky Node"

-- runNode config state logger
