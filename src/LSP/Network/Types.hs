{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module LSP.Network.Types
  ( ServerApp
  , ServerEnv (..)
  , ServerConfig (..)
  , runServerApp

  , ClientApp
  , ClientEnv (..)
  , ClientConfig (..)
  , runClientApp

  , Project (..)
  , findProjectByPath
  , findProjectByRedundantPath
  ) where

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.Aeson                  (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                  as Aeson
import           Data.List                   (isPrefixOf)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           GHC.Generics                (Generic)
import           Network.Socket              (HostName)

import qualified LSP.Network.Logging         as Log

-------------------------------------------------------------------------------
-- Server

data ServerConfig = ServerConfig
  { host         :: !HostName
  , port         :: !Int
  , serverLogger :: !Log.Logger
  } deriving (Generic)

instance FromJSON ServerConfig where
  parseJSON =
    let flm = \case
          "host"         -> "host"
          "port"         -> "port"
          "serverLogger" -> "logger"
          x              -> x
        otps = Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }
     in Aeson.genericParseJSON otps

data ServerEnv m = ServerEnv
  { serverConfig    :: ServerConfig
  , serverLogAction :: Log.LogAction m Log.Message
  }

newtype ServerApp a = ServerApp
  { unServerApp :: ReaderT (ServerEnv ServerApp) IO a }
  deriving newtype ( Functor, Applicative, Monad, MonadIO
                   , MonadReader (ServerEnv ServerApp), MonadBase IO
                   )

instance MonadBaseControl IO ServerApp where
  type StM ServerApp a = a
  liftBaseWith f = ServerApp $ liftBaseWith $ \x -> f (x . unServerApp)
  restoreM = ServerApp . restoreM

instance Log.HasLog (ServerEnv m) Log.Message m where
  getLogAction :: ServerEnv m -> Log.LogAction m Log.Message
  getLogAction = serverLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: Log.LogAction m Log.Message -> ServerEnv m -> ServerEnv m
  setLogAction action env = env{serverLogAction = action}
  {-# INLINE setLogAction #-}

runServerApp :: ServerEnv ServerApp -> ServerApp a -> IO a
runServerApp env app = runReaderT (unServerApp app) env

-------------------------------------------------------------------------------
-- Client

data ClientConfig = ClientConfig
  { serverHost     :: !HostName
  , serverPort     :: !Int
  , clientProjects :: !(Vector Project)
  , clientLogger   :: !Log.Logger
  } deriving (Generic)

instance FromJSON ClientConfig where
  parseJSON =
    let flm = \case
          "serverHost"     -> "host"
          "serverPort"     -> "port"
          "clientProjects" -> "projects"
          "clientLogger"   -> "logger"
          x                -> x
        otps = Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }
     in Aeson.genericParseJSON otps

data ClientEnv m = ClientEnv
  { clientConfig    :: ClientConfig
  , clientLogAction :: Log.LogAction m Log.Message
  }

newtype ClientApp a = ClientApp
  { unClientApp :: ReaderT (ClientEnv ClientApp) IO a }
  deriving newtype ( Functor, Applicative, Monad, MonadIO
                   , MonadReader (ClientEnv ClientApp), MonadBase IO
                   )

instance MonadBaseControl IO ClientApp where
  type StM ClientApp a = a
  liftBaseWith f = ClientApp $ liftBaseWith $ \x -> f (x . unClientApp)
  restoreM = ClientApp . restoreM

instance Log.HasLog (ClientEnv m) Log.Message m where
  getLogAction :: ClientEnv m -> Log.LogAction m Log.Message
  getLogAction = clientLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: Log.LogAction m Log.Message -> ClientEnv m -> ClientEnv m
  setLogAction action env = env{clientLogAction = action}
  {-# INLINE setLogAction #-}

runClientApp :: ClientEnv ClientApp -> ClientApp a -> IO a
runClientApp env app = runReaderT (unClientApp app) env

-------------------------------------------------------------------------------

data Project = Project
  { projectRoot          :: FilePath
  , projectServerCommand :: FilePath
  , projectServerCmdArgs :: [String]
  } deriving (Show, Generic)

projectJsonOptions :: Aeson.Options
projectJsonOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }
  where
    flm "projectRoot"          = "root"
    flm "projectServerCommand" = "command"
    flm "projectServerCmdArgs" = "args"
    flm x                      = x

instance FromJSON Project where
  parseJSON = Aeson.genericParseJSON projectJsonOptions

instance ToJSON Project where
  toJSON = Aeson.genericToJSON projectJsonOptions

findProjectByPath :: FilePath -> Vector Project -> Maybe Project
findProjectByPath path = V.find (\p -> projectRoot p == path)

findProjectByRedundantPath :: FilePath -> Vector Project -> Maybe Project
findProjectByRedundantPath path = V.find (\p -> projectRoot p `isPrefixOf` path)
