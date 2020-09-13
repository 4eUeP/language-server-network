{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Concurrent            (forkIO)
import           Data.ByteString               as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import           Data.Yaml.Config              (loadYamlSettingsArgs, useEnv)
import           Network.Socket.ByteString     (recv, sendAll)
import           System.Directory              (getCurrentDirectory)
import qualified System.IO                     as IO

import           LSP.Network                   (connect, sendJSON)
import           LSP.Network.Types             (ClientConfig (..),
                                                findProjectByRedundantPath)
import           LSP.Network.Utils             (untilM_)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  ClientConfig{..} <- loadYamlSettingsArgs [] useEnv

  IO.hSetBuffering IO.stdin  IO.NoBuffering
  IO.hSetEncoding  IO.stdin  IO.utf8
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetEncoding  IO.stdout IO.utf8

  connect serverHost (show serverPort) $ \(sock, _addr) -> do
    -- FIXME: we use this current directory as the id of project,
    -- it may be a sub directory of real project root.
    cwd <- getCurrentDirectory
    case findProjectByRedundantPath cwd clientProjects of
      Nothing -> do
        -- TODO
        let errmsg = "No such project: " <> cwd <> "\n"
        Prelude.appendFile "/tmp/lsp-network.log" $ errmsg
      Just project -> do
        sendJSON sock project

        _ <- forkIO $ do
          untilM_ (BS.hGetSome IO.stdin defaultChunkSize) $ \cin ->
            if BS.null cin
               then return False
               else sendAll sock cin >> return True

        untilM_ (recv sock 1024) $ \sout ->
          if BS.null sout
             then return False
             else do BS.hPut IO.stdout sout
                     return True
