{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LSP.Network.Logging
  ( Logger (..)

  , stdoutLogAction
  , withFileLogAction

  , Colog.HasLog (..)
  , Colog.LogAction
  , Colog.Message

  , Colog.logDebug
  , Colog.logInfo
  , Colog.logWarning
  , Colog.logError
  , Colog.logException
  ) where

import qualified Colog                  as Colog
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (..), (.:))
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Types       as Aeson
import qualified Data.HashMap.Strict    as HMap
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as TIO
import           GHC.Generics           (Generic)
import           System.IO              (Handle, IOMode (..), hFlush, withFile)

-------------------------------------------------------------------------------

data Logger
  = LogStdoutHandler LogStdout
  | LogFileHandler   LogFile
  deriving (Generic)

instance FromJSON Logger where
  parseJSON = Aeson.withObject "LoggerHandler" $ \v -> do
    handler <- v .: "handler" :: Aeson.Parser Text
    let v' = Aeson.Object (HMap.delete "handler" v)
    case handler of
      "stdout" -> LogStdoutHandler <$> (Aeson.parseJSON $ v')
      "file"   -> LogFileHandler   <$> (Aeson.parseJSON $ v')
      x        -> fail $ "Invalid log handler: " <> Text.unpack x

data LogStdout = LogStdout
  { stdoutFormatter :: LoggerFormatter
  , stdoutLevel     :: LoggerLevel
  } deriving (Generic)

instance FromJSON LogStdout where
  parseJSON =
    let flm = \case
          "stdoutFormatter" -> "formatter"
          "stdoutLevel"     -> "level"
          x                 -> x
     in Aeson.genericParseJSON $
          Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }

data LogFile = LogFile
  { logfilePath   :: FilePath
  , fileFormatter :: LoggerFormatter
  , fileLevel     :: LoggerLevel
  } deriving (Generic)

instance FromJSON LogFile where
  parseJSON =
    let flm = \case
          "logfilePath"   -> "filename"
          "fileFormatter" -> "formatter"
          "fileLevel"     -> "level"
          x               -> x
     in Aeson.genericParseJSON $
          Aeson.defaultOptions { Aeson.fieldLabelModifier = flm }

data LoggerFormatter = RichFormatter | SimpleFormatter
  deriving (Generic)

instance FromJSON LoggerFormatter where
  parseJSON =
    Aeson.withText "LoggerFormatter" $ \case
      "rich"   -> return RichFormatter
      "simple" -> return SimpleFormatter
      x        -> fail $ "Invalid log formatter: " <> Text.unpack x

data LoggerLevel = DebugLevel | InfoLevel | WarningLevel | ErrorLevel
  deriving (Generic)

instance FromJSON LoggerLevel where
  parseJSON =
    Aeson.withText "LoggerLevel" $ \case
      "debug"   -> return DebugLevel
      "info"    -> return InfoLevel
      "warning" -> return WarningLevel
      "error"   -> return ErrorLevel
      x         -> fail $ "Invalid log level: " <> Text.unpack x

-------------------------------------------------------------------------------
-- LogAction

stdoutLogAction
  :: MonadIO m
  => LogStdout
  -> Colog.LogAction m Colog.Message
stdoutLogAction LogStdout{..} =
  case stdoutFormatter of
    SimpleFormatter -> filterLogByLevel Colog.simpleMessageAction stdoutLevel
    RichFormatter   -> filterLogByLevel Colog.richMessageAction stdoutLevel

withFileLogAction
  :: MonadIO m
  => LogFile
  -> (Colog.LogAction m Colog.Message -> IO r)
  -> IO r
withFileLogAction LogFile{..} action =
  case fileFormatter of
    SimpleFormatter ->
      withFile logfilePath AppendMode $ action . filterLog . logMessageHandle
    RichFormatter   ->
      withFile logfilePath AppendMode $ action . filterLog . logRichMessageHandle
  where
    filterLog = flip filterLogByLevel fileLevel

-------------------------------------------------------------------------------

logMessageHandle :: MonadIO m => Handle -> Colog.LogAction m Colog.Message
logMessageHandle handle =
  Colog.cmap Colog.fmtMessage (logTextFlushHandle handle)

logRichMessageHandle :: MonadIO m => Handle -> Colog.LogAction m Colog.Message
logRichMessageHandle handle =
  Colog.upgradeMessageAction Colog.defaultFieldMap $
    Colog.cmapM Colog.fmtRichMessageDefault (logTextFlushHandle handle)

-- See: https://github.com/kowainik/co-log/issues/176
logTextFlushHandle :: MonadIO m => Handle -> Colog.LogAction m Text
logTextFlushHandle handle = Colog.LogAction $ \msg -> liftIO $ do
  TIO.hPutStrLn handle msg
  hFlush handle

filterLogByLevel
  :: MonadIO m
  => Colog.LogAction m Colog.Message
  -> LoggerLevel
  -> (Colog.LogAction m Colog.Message)
filterLogByLevel action = \case
  DebugLevel   -> Colog.filterBySeverity Colog.D Colog.msgSeverity action
  InfoLevel    -> Colog.filterBySeverity Colog.I Colog.msgSeverity action
  WarningLevel -> Colog.filterBySeverity Colog.W Colog.msgSeverity action
  ErrorLevel   -> Colog.filterBySeverity Colog.E Colog.msgSeverity action
