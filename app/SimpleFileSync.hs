{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar,
                                          tryPutMVar)
import           Control.Exception.Safe  (tryAny)
import           Control.Monad
import           Data.Aeson              (FromJSON)
import           Data.List               (intercalate)
import           Data.Maybe              (fromMaybe)
import           Data.Yaml               (decodeFileEither,
                                          prettyPrintParseException)
import           GHC.Generics            (Generic)
import           System.Console.GetOpt
import           System.Directory        (canonicalizePath, doesDirectoryExist)
import           System.Environment      (getArgs)
import           System.Exit
import           System.FilePath         (addTrailingPathSeparator, (</>))
import           System.FilePattern      ((?==))
import qualified System.FSNotify         as FSN
import qualified System.FSNotify.Devel   as FSN
import qualified System.Process          as Proc

-------------------------------------------------------------------------------

data Project = Project
  { src_path   :: !String
  , dest_path  :: !String
  , ignores    :: ![String]
  , rsync_opts :: ![String]
  , debug      :: !(Maybe Bool)
  } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
  (opts, _) <- getOpts =<< getArgs
  config <- loadConfig (optConfigFile opts)
  projects <- mapM (\p -> (p, ) <$> newEmptyMVar) config
  case projects of
    [] -> errorWithoutStackTrace "Empty project!"
    (proj:ps) -> FSN.withManager $ \mgr -> do
      putStrLn "Starting watching threads..."
      mapM_ (\(p, f) -> watchProject f p mgr) projects
      putStrLn "Starting synchronizing threads..."
      mapM_ (\(p, f) -> forkIO $ foreverRun 200000 $ runRsync f p) ps
      -- we pick our first project to run on main thread
      foreverRun 200000 $ runRsync (snd proj) (fst proj)

-------------------------------------------------------------------------------

data Options = Options
  { optConfigFile :: !FilePath
  , optHelp       :: !Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optConfigFile = "config.yaml"
  , optHelp = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["config"]
      (ReqArg (\arg opts -> opts{ optConfigFile = arg })
              "FILE")
      "The config file in yaml format, default is config.yaml"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts{ optHelp = True }))
      "Print help message"
  ]

getOpts :: [String] -> IO (Options, [String])
getOpts argv =
  case getOpt Permute options argv of
    (o, n, []  ) -> do
      let r@(opts, _) = (foldl (flip id) defaultOptions o, n)
      when (optHelp opts) $ do putStrLn $ usageInfo header options
                               exitWith ExitSuccess
      pure r
    (_, _, errs) -> errorWithoutStackTrace $ concat errs ++ usageInfo header options
  where
    header = "Usage: simple-file-sync"

loadConfig :: FilePath -> IO [Project]
loadConfig fp = do
  e <- decodeFileEither fp
  case e of
    Left ex -> errorWithoutStackTrace (prettyPrintParseException ex)
    Right x -> pure x

-------------------------------------------------------------------------------

watchProject :: MVar () -> Project -> FSN.WatchManager -> IO ()
watchProject flagChange Project{..} mgr = do
  ignores' <- canonicalizeIgnores
  -- TODO: performance improvements (if needed)
  void $ FSN.watchTree mgr
                       src_path
                       (FSN.allEvents $ \path -> not $ any (?== path) ignores')
                       (FSN.doAllEvents action)
  where
    action path = do
      when (fromMaybe False debug) $ putStrLn $ "File changed: " <> show path
      void $ tryPutMVar flagChange ()
    -- TODO: logic improvements
    canonicalizeIgnores = do
      base <- canonicalizePath src_path
      forM ignores $ \i -> do
        let i' = base </> i
        e <- doesDirectoryExist i'
        if e then pure $ i' </> "**" else pure $ i'

runRsync :: MVar () -> Project -> IO ()
runRsync flagChange Project{..} = do
  let srcPath' = addTrailingPathSeparator src_path
      destPath' = addTrailingPathSeparator dest_path
  let args = ["-azH", "--delete", "--partial"]
          ++ concatMap (\i -> ["--exclude", i]) ignores
          ++ concatMap words rsync_opts
          ++ [srcPath', destPath']
  takeMVar flagChange
  when (fromMaybe False debug) $ putStrLn $ "Run command: " <> "rsync " <> (intercalate " " args)
  (_code, _out, err) <- Proc.readProcessWithExitCode "rsync" args ""
  unless (null err) $ putStrLn $ "Run rsync error: " <> err

-------------------------------------------------------------------------------

foreverRun :: Int -> IO () -> IO ()
foreverRun delay = run (maxN - 1)
  where
    maxN = 60 :: Double
    run 0 _ = error "Error with all retries used!"
    run !n f = do
      when (delay > 0) (threadDelay delay)
      result <- tryAny f
      case result of
        Left e -> do putStrLn $ "foreverRun error: " <> (show e)
                     -- Retry delay
                     threadDelay $ ceiling $ (maxN - n) * log (maxN - n) * 1000000
                     run (n - 1) f
        Right _ -> run 60 f
