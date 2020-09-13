{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module LSP.Network.Types
  ( ServerConfig (..)
  , ClientConfig (..)

  , Project (..)
  , findProjectByPath
  , findProjectByRedundantPath
  ) where

import           Data.Aeson     (FromJSON (..), ToJSON (..))
import qualified Data.Aeson     as Aeson
import           Data.List      (isPrefixOf)
import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           GHC.Generics   (Generic)
import           Network.Socket (HostName)

-------------------------------------------------------------------------------

data ServerConfig = ServerConfig
  { host :: !HostName
  , port :: !Int
  } deriving (Generic, FromJSON)

data ClientConfig = ClientConfig
  { serverHost     :: !HostName
  , serverPort     :: !Int
  , clientProjects :: !(Vector Project)
  } deriving (Generic)

clientJSONOptions :: Aeson.Options
clientJSONOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }
  where
    flm "serverHost"     = "host"
    flm "serverPort"     = "port"
    flm "clientProjects" = "projects"
    flm x                = x

instance FromJSON ClientConfig where
  parseJSON = Aeson.genericParseJSON clientJSONOptions

instance ToJSON ClientConfig where
  toJSON = Aeson.genericToJSON clientJSONOptions

data Project = Project
  { projectRoot          :: FilePath
  , projectServerCommand :: FilePath
  , projectServerCmdArgs :: [String]
  } deriving (Generic)

projectJsonOptions :: Aeson.Options
projectJsonOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }
  where
    flm "projectRoot"          = "path"
    flm "projectServerCommand" = "server-cmd"
    flm "projectServerCmdArgs" = "server-args"
    flm x                      = x

instance FromJSON Project where
  parseJSON = Aeson.genericParseJSON projectJsonOptions

instance ToJSON Project where
  toJSON = Aeson.genericToJSON projectJsonOptions

-------------------------------------------------------------------------------

findProjectByPath :: FilePath -> Vector Project -> Maybe Project
findProjectByPath path = V.find (\p -> projectRoot p == path)

findProjectByRedundantPath :: FilePath -> Vector Project -> Maybe Project
findProjectByRedundantPath path = V.find (\p -> projectRoot p `isPrefixOf` path)
