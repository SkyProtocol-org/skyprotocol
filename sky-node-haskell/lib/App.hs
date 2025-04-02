{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App (initHTTPApp, initSocketApp) where

import App.Env
import Config (AppConfig (..))
import Control.Monad (forever)
import Crypto.PubKey.RSA (PublicKey)
import Data.Text (pack, unpack)
import Effectful hiding ((:>))
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Error.Static (Error, runError)
import Effectful.Log
import Effectful.Reader.Static
import Network.Socket qualified as S
-- import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Types (BlockData, BlockId, TopicId)
import UnliftIO.Exception (bracket)
import Utils

type TopicAPI =
  "topics"
    :> ( "list-all" :> Get '[JSON] [TopicId]
           :<|> "add" :> ReqBody '[JSON] [PublicKey] :> Post '[JSON] Bool
           :<|> Capture "topicId" TopicId :> "committee" :> Get '[JSON] [PublicKey]
       )

type BlockAPI =
  "blocks"
    :> ( Capture "topicId" TopicId :> Capture "blockId" BlockId :> Get '[JSON] BlockData
           :<|> "add" :> ReqBody '[JSON] BlockData :> Post '[JSON] Bool
       )

type SkyNodeAPI = TopicAPI :<|> BlockAPI

skyNodeServer :: Server SkyNodeAPI
skyNodeServer = topicAPIH :<|> blockAPIH
  where
    topicAPIH = undefined
    blockAPIH = undefined

initHTTPApp :: AppConfig -> Logger -> IO ()
initHTTPApp config logger = undefined

type AppEffects = '[Reader AppEnv, Concurrent, Error String, Log, IOE]

initSocketApp :: AppConfig -> Logger -> IO ()
initSocketApp config logger = do
  env <- initAppEnv config
  runEff $ do
    runLog "main" logger defaultLogLevel $ do
      -- we don't want handlers to interrupt the node with it's exceptions
      eRes <- runError $
        runConcurrent $
          runReader env $ do
            withSocketServer handlePeer
      case eRes of
        Left err -> logAttention_ $ pack $ show err
        Right res -> logInfo_ $ pack $ show res

withSocketServer :: (S.Socket -> Eff AppEffects ()) -> Eff AppEffects ()
withSocketServer handler = do
  logInfo_ "Starting Sky Node..."
  config <- askFieldS @AppConfig
  bracket setupServerSocket (liftIO . S.close) $ \sock -> do
    logInfo_ $ "Node listening on " <> config.host <> ":" <> config.port
    forever $ do
      (conn, addr) <- liftIO $ S.accept sock
      logInfo_ $ "Connection accepted from " <> pack (show addr)
      -- using withAsync ensures proper thread handling in the face of execptions
      withAsync (handler conn) $ \_ -> pure ()

setupServerSocket :: Eff AppEffects S.Socket
setupServerSocket = do
  h <- (.host) <$> askFieldS @AppConfig
  p <- (.port) <$> askFieldS @AppConfig
  addrs <- liftIO $ S.getAddrInfo (Just S.defaultHints {S.addrSocketType = S.Stream}) (Just $ unpack h) (Just $ unpack p)
  let addr = head addrs
  sock <- liftIO $ S.openSocket addr
  liftIO $ do
    S.setSocketOption sock S.ReuseAddr 1 -- easier for debugging
    S.bind sock (S.addrAddress addr)
    S.listen sock 10
  pure sock

-- TODO future implementation with sockets
handlePeer :: S.Socket -> Eff AppEffects ()
handlePeer sock = undefined

-- runPeerEffIO sock $ do
-- undefined
