{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Concurrent            (forkIO)
import           Control.Exception             (SomeException, try)
import           Control.Monad                 (void, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (ask)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.Text                     as Text
import           Data.Yaml.Config              (loadYamlSettingsArgs, useEnv)
import           Network.Socket                (Socket)
import           Network.Socket.ByteString     (recv, sendAll)
import           System.IO                     (Handle)
import qualified System.IO                     as IO

import           LSP.Network                   (recvJSON, runLangServer, serve)
import qualified LSP.Network.Logging           as Log
import qualified LSP.Network.Types             as T
import           LSP.Network.Utils             (retry, textShow, untilM_)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  config@T.ServerConfig{..} <- loadYamlSettingsArgs [] useEnv
  case serverLogger of
    Log.LogStdoutHandler h ->
      let env = T.ServerEnv config (Log.stdoutLogAction h)
       in T.runServerApp env runServer
    Log.LogFileHandler h -> do
      Log.withFileLogAction h $ \action ->
        let env = T.ServerEnv config action
         in T.runServerApp env runServer

runServer :: T.ServerApp ()
runServer = do
  env <- ask
  let config = T.serverConfig env
  let h = T.host config
      p = show $ T.port config

  Log.logInfo "--------------------- LSP-network Server ---------------------"
  Log.logInfo $ "Listening on " <> Text.pack h <> ":" <> Text.pack p

  serve h p $ \(sock, _addr) -> do
    (e_config, m_rest) <- recvJSON sock
    case e_config of
      Left errmsg -> Log.logError $ "Parse error: " <> Text.pack errmsg
      Right project -> runLSP sock project m_rest

runLSP :: Socket -> T.Project -> Maybe ByteString -> T.ServerApp ()
runLSP sock project m_rest = do
  env <- ask
  void $ retry (run env) (const True) 10 (10^(6 :: Int))
  where
    run env = do
      Log.logInfo $ "Run LangServer with " <> (Text.pack $ show project)
      runLangServer project $ \hin hout -> do
        void . forkIO $ T.runServerApp env (clientIn hin sock m_rest)
        T.runServerApp env (serverOut hout sock)

clientIn :: Handle -> Socket -> Maybe ByteString -> T.ServerApp ()
clientIn hin sock m_initBS = do
  maybe (pure ()) tryWrite m_initBS

  untilM_ (liftIO $ recv sock 1024) $ \cin ->
    if BS.null cin
       then do Log.logInfo "Client closed."
               return False
       else do tryWrite cin
               return True
  where
    tryWrite bs = do
      isOpen <- liftIO $ IO.hIsOpen hin
      isWritable <- liftIO $ IO.hIsWritable hin
      when (isOpen && isWritable) $ do
        Log.logDebug $ "ClientIn: " <> Text.pack (show bs)
        e_r <- liftIO $ try $ BS.hPut hin bs
        case e_r of
          Left e -> Log.logException (e :: SomeException)
          _      -> return ()
    {-# INLINE tryWrite #-}

serverOut :: Handle -> Socket -> T.ServerApp ()
serverOut hout sock = do
  isOpen <- liftIO $ IO.hIsOpen hout
  when isOpen go
  where
    tryRead = do
      isReadable <- liftIO $ IO.hIsReadable hout
      Log.logDebug $ "Is server command stdout readable: " <> textShow isReadable
      m_rs <- liftIO $ retry (BS.hGetSome hout defaultChunkSize)
                             (not . BS.null) 10 (10^(6 :: Int))
                             -- try read 10 times for every 1 second.
      case m_rs of
        Nothing -> return BS.empty
        Just rs -> return rs

    go = untilM_ tryRead $ \sout ->
      if BS.null sout
         then do Log.logInfo $ "Reach EOF."
                 return False
         else do Log.logDebug $ "ServerReply: " <> Text.pack (show sout)
                 liftIO $ sendAll sock sout
                 return True
