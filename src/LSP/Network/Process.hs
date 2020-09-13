{-# LANGUAGE RecordWildCards #-}

module LSP.Network.Process
  ( runLangServer
  ) where

import           Control.Monad     (liftM2)
import           System.Directory  (doesDirectoryExist, findExecutable)
import           System.IO         (Handle)
import qualified System.IO         as IO
import qualified System.Process    as Proc

import           LSP.Network.Types (Project (..))

-------------------------------------------------------------------------------

runLangServer :: Project -> (Handle -> Handle -> IO a) -> IO a
runLangServer p@Project{..} hdl = do
  checkResult <- checkProject p
  case checkResult of
    Left errmsg -> error errmsg
    Right _ -> do
      let process =
            (Proc.proc projectServerCommand projectServerCmdArgs)
              { Proc.std_out = Proc.CreatePipe
              , Proc.std_in  = Proc.CreatePipe
              , Proc.cwd     = Just projectRoot
              }
      Proc.withCreateProcess process processing
  where
    processing (Just hin) (Just hout) _herr _ph = do
      IO.hSetBuffering hin IO.NoBuffering
      IO.hSetEncoding  hin IO.utf8
      IO.hSetBuffering hout IO.NoBuffering
      IO.hSetEncoding  hout IO.utf8
      hdl hin hout
    processing _ _ _ _ = error "Create process error."

-------------------------------------------------------------------------------

checkProject :: Project -> IO (Either String ())
checkProject project =
  liftM2 (>>) (projectRootMustExist project)
              (serverCommandMustExist project)

projectRootMustExist :: Project -> IO (Either String ())
projectRootMustExist Project{projectRoot=path} = do
  exist <- doesDirectoryExist path
  return $ if exist then Right () else Left (path <> " does not exist!")

serverCommandMustExist :: Project -> IO (Either String ())
serverCommandMustExist Project{projectServerCommand=cmd} = do
  m_exe <- findExecutable cmd
  case m_exe of
    Nothing -> return $ Left ("No such executable file: " <> cmd)
    Just _  -> return $ Right ()
