{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Concurrent            (forkIO)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import           Data.Yaml.Config              (loadYamlSettingsArgs, useEnv)
import           Network.Socket.ByteString     (recv, sendAll)
import           System.IO                     (Handle)

import           LSP.Network                   (recvJSON, runLangServer,
                                                serve)
import           LSP.Network.Types             (ServerConfig (..))
import           LSP.Network.Utils             (untilM_)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  ServerConfig{..} <- loadYamlSettingsArgs [] useEnv

  serve host (show port) $ \(sock, _addr) -> do
    (e_config, m_rest) <- recvJSON sock
    case e_config of
      Left errmsg -> Prelude.putStrLn $ "Parse error: " <> errmsg
      Right project -> do
        runLangServer project $ \hin hout -> do
          _ <- forkIO $ do
            maybe (pure ()) (onRecv hin) m_rest

            untilM_ (recv sock 1024) $ \cin ->
              if BS.null cin
                 then do Prelude.putStrLn $ "Client closed."
                         return False
                 else do onRecv hin cin
                         return True

          untilM_ (BS.hGetSome hout defaultChunkSize) $ \sout ->
            if BS.null sout
               then do Prelude.putStrLn $ "Reach EOF."
                       return False
               else do Prelude.putStrLn $ "ServerReply: " <> show sout
                       sendAll sock sout
                       return True

onRecv :: MonadIO m => Handle -> ByteString -> m ()
onRecv hin input = liftIO $ do
  Prelude.putStrLn $ "ClientIn: " <> show input
  BS.hPut hin input
