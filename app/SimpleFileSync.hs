{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar,
                                          tryPutMVar)
import           Control.Exception.Safe  (tryAny)
import           Control.Monad           (forM, unless, void)
import           Control.Monad.Extra     (anyM, unlessM)
import           Data.List               (isInfixOf)
import           GHC.Generics            (Generic)
import           System.FilePattern      ((?==))
import qualified Z.Data.Builder          as Builder
import qualified Z.Data.CBytes           as CBytes
import           Z.Data.JSON             (JSON)
import qualified Z.Data.Text             as Text
import           Z.Data.Text             (Text)
import qualified Z.Data.Vector           as V
import           Z.Data.YAML             (readYAMLFile)
import           Z.IO.Environment        (getArgs)
import qualified Z.IO.FileSystem         as FS
import qualified Z.IO.Logger             as Log
import           Z.IO.LowResTimer        (throttleTrailing_)
import qualified Z.IO.Process            as Proc

data Project = Project
  { src_path   :: Text
  , dest_path  :: Text
  , rsync_opts :: [Text]
  , ignores    :: [Text]
  } deriving (Show, Generic, JSON)

main :: IO ()
main = Log.withDefaultLogger $ do
  argv <- getArgs
  if length argv /= 2
     then Log.fatal $ "No sush config file, run "
                   <> CBytes.toBuilder (head argv) <> " <your-config-file-path>."
     else do
       let configPath = argv !! 1
       config <- readYAMLFile configPath
       projects <- mapM (\p -> (p, ) <$> newEmptyMVar) config
       case projects of
         [] -> Log.fatal "Empty project!"
         first:ps -> do
          Log.info "Starting watching threads..."
          mapM_ (\(p, f) -> forkIO $ foreverRun $ watchProject f p) projects
          Log.info "Starting synchronizing threads..."
          mapM_ (\(p, f) -> forkIO $ foreverRun $ runRsync f p) ps
          -- we pick our first project to run on main thread
          foreverRun $ runRsync (snd first) (fst first)

watchProject :: MVar () -> Project -> IO ()
watchProject flagChange Project{..} = do
  -- no matter how many FileChange events are popped, we will notify only once
  -- after (2/10)s.
  FS.watchDirs [CBytes.fromText src_path] True $ notify
  where
    getPath (FS.FileAdd path)    = path
    getPath (FS.FileRemove path) = path
    getPath (FS.FileModify path) = path

    isSub (_, segs1) (_, segs2) = pure $ isInfixOf segs2 segs1

    -- FIXME: the same as rsync excludes
    filterRule fe = do
      let file = getPath fe
      file_segs <- FS.splitSegments file
      -- NOTE: here we only need the directory in the ingores, but we can't
      -- detect it, since the directory can be any subdirs.
      ign_segs <- forM ignores (FS.splitSegments . CBytes.fromText)
      isSubContents <- anyM (isSub file_segs) ign_segs
      if isSubContents
         then pure True
         else do
           let igs_pat = map (\i -> Text.unpack $ "**/" <> i) ignores
               doesMatchPat = any (?== CBytes.unpack file) igs_pat
           unless doesMatchPat $
             Log.debug $ "File changed: " <> Builder.stringUTF8 (show fe)
                      <> ", ignore patterns: " <> Builder.stringUTF8 (show igs_pat)
           pure doesMatchPat

    -- no matter how many FileChange events are popped, we will notify only
    -- once after (2/10)s.
    notify e = do
      action <- throttleTrailing_ 2 $
        unlessM (filterRule e) $ void $ tryPutMVar flagChange ()
      action

runRsync :: MVar () -> Project -> IO ()
runRsync flagChange Project{..} = do
  src_path' <- (<> "/") <$> FS.normalize (CBytes.fromText src_path)     -- add a trailing slash
  dest_path' <- (<> "/") <$> FS.normalize (CBytes.fromText dest_path)   -- add a trailing slash
  let args = ["-azH", "--delete", "--partial"]
          ++ concatMap (map CBytes.fromText . Text.words) rsync_opts
          ++ concatMap (\i -> ["--exclude", CBytes.fromText i]) ignores
          ++ [src_path', dest_path']
  takeMVar flagChange
  Log.debug $ "Run command: " <> "rsync "
           <> CBytes.toBuilder (CBytes.intercalate " " args)
  (_out, err, _code) <- Proc.readProcess
    Proc.defaultProcessOptions { Proc.processFile = "rsync"
                               , Proc.processArgs = args
                               }
    ""
  unless (V.null err) $ Log.fatal $ Builder.bytes err

foreverRun :: IO () -> IO ()
foreverRun = run (maxN - 1)
  where
    maxN = 60
    run 0 _ = error "Error with all retries used!"
    run n f = do
      result <- tryAny f
      case result of
        Left e -> do Log.fatal $ Builder.stringUTF8 (show e)
                     threadDelay $ ceiling $ (maxN - n) * log (maxN - n) * 10^6
                     run (n - 1) f
        Right _ -> run 60 f
