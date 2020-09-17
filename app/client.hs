{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (ask)
import           Data.ByteString               as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.Text                     as Text
import           Data.Yaml.Config              (loadYamlSettingsArgs, useEnv)
import           Network.Socket                (Socket)
import           Network.Socket.ByteString     (recv, sendAll)
import           System.Directory              (getCurrentDirectory)
import qualified System.IO                     as IO

import           LSP.Network                   (connect, sendJSON)
import qualified LSP.Network.Logging           as Log
import qualified LSP.Network.Types             as T
import           LSP.Network.Utils             (untilM_)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin  IO.NoBuffering
  IO.hSetEncoding  IO.stdin  IO.utf8
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetEncoding  IO.stdout IO.utf8

  config@T.ClientConfig{..} <- loadYamlSettingsArgs [] useEnv
  case clientLogger of
    Log.LogFileHandler h -> do
      Log.withFileLogAction h $ \action ->
        let env = T.ClientEnv config action
         in T.runClientApp env runClient
    _ -> error "Only log to file on client side!"

runClient :: T.ClientApp ()
runClient = do
  env <- ask
  let config = T.clientConfig env
  let h = T.serverHost config
      p = show $ T.serverPort config
  let projects = T.clientProjects config

  Log.logInfo "--------------------- LSP-network Client ---------------------"
  Log.logInfo $ "Connecting to " <> Text.pack h <> ":" <> Text.pack p

  connect h p $ \(sock, _addr) -> do
    -- FIXME: we use this current directory as the id of project,
    -- it may be a sub directory of real project root.
    cwd <- liftIO $ getCurrentDirectory
    case T.findProjectByRedundantPath cwd projects of
      Nothing -> do
        Log.logError $ "No such project: " <> (Text.pack cwd)
      Just project -> do
        sendJSON sock project
        liftIO $ void . forkIO $ T.runClientApp env (msgIn sock)
        msgOut sock

msgIn :: Socket -> T.ClientApp ()
msgIn sock = do
  untilM_ (liftIO $ BS.hGetSome IO.stdin defaultChunkSize) $ \cin ->
    if BS.null cin
       then do Log.logInfo $ "Reach EOF."
               return False
       else do liftIO $ sendAll sock cin
               return True

msgOut :: Socket -> T.ClientApp ()
msgOut sock = do
  untilM_ (liftIO $ recv sock 1024) $ \sout ->
    if BS.null sout
       then do Log.logInfo "Server closed."
               return False
       else do liftIO $ BS.hPut IO.stdout sout
               return True
