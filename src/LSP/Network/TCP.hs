{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSP.Network.TCP
  ( serve
  , serve'
  , connect
  , sendJSON
  , recvJSON
  ) where

import qualified Control.Concurrent.Lifted      as L
import qualified Control.Exception              as Ex
import           Control.Monad                  (forever, void)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Control    (MonadBaseControl, liftBaseOp)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Aeson                     as Aeson
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BSC
import qualified Data.ByteString.Lazy           as BSL
import           Network.Socket                 (HostName, ServiceName, Socket)
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NB
import qualified Network.Socket.ByteString.Lazy as NBL
import qualified Scanner                        as P
import qualified System.IO                      as IO

import           LSP.Network.Utils              (toMaybe)

-------------------------------------------------------------------------------

serve
  :: (MonadBaseControl IO m, MonadIO m)
  => HostName
  -> ServiceName
  -> ((Socket, NS.SockAddr) -> m ())
  -> m a
serve host port =
  let x = "LSP.Network.TCP.serve: Synchronous exception happened: "
      clean' = liftIO . uncurry (flip $ clean x)
   in serve' host port setDefaultServerSO clean'

serve'
  :: (MonadBaseControl IO m, MonadIO m)
  => HostName
  -> ServiceName
  -> (Socket -> IO ())   -- ^ set socket options
  -> ((Either Ex.SomeException (), Socket) -> m ())
  -> ((Socket, NS.SockAddr) -> m ())
  -> m a
serve' host port setOpts release server = do
  addr <- resolve host port True
  gbracket (openServer addr setOpts) close (acceptConc server release)

connect
  :: (MonadBaseControl IO m)
  => HostName
  -> ServiceName
  -> ((Socket, NS.SockAddr) -> m r)
  -> m r
connect host port = gbracket (connectSock host port) (close . fst)

sendJSON :: (MonadIO m, ToJSON a) => Socket -> a -> m ()
sendJSON sock conf =
  let datas = Aeson.encode conf
      len = pack $ BSL.length datas  -- N.B. this len must not exceed (maxBound :: Int64)
      payload = BSL.concat [BSL.fromStrict len, "\n", datas, "\n"]
   in liftIO $ NBL.sendAll sock payload

recvJSON :: (MonadIO m, FromJSON a) => Socket -> m (Either String a, Maybe ByteString)
recvJSON sock = go (P.scan parser)
  where
    go next = do
      bs <- liftIO $ NB.recv sock 1024
      case next bs of
        P.More next' -> go next'
        P.Done rest r -> return (Aeson.eitherDecodeStrict' r, toMaybe (BS.null rest) rest)
        P.Fail _ errmsg -> return (Left errmsg, Nothing)
    parser :: P.Scanner ByteString
    parser = do
      len <- P.decimal <* P.char8 '\n'
      P.take len <* P.char8 '\n'

-------------------------------------------------------------------------------

setDefaultServerSO :: Socket -> IO ()
setDefaultServerSO sock = do
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.setSocketOption sock NS.NoDelay 1
  NS.setSocketOption sock NS.KeepAlive 1
  NS.withFdSocket sock NS.setCloseOnExecIfNeeded
{-# INLINE setDefaultServerSO #-}

setDefaultClientSO :: Socket -> IO ()
setDefaultClientSO sock = do
  NS.setSocketOption sock NS.NoDelay 1
  NS.setSocketOption sock NS.KeepAlive 1
{-# INLINE setDefaultClientSO #-}

resolve :: MonadIO m => HostName -> ServiceName -> Bool -> m NS.AddrInfo
resolve host port passive =
  let flags = if passive then [NS.AI_PASSIVE] else [NS.AI_ADDRCONFIG]
      hints =
        NS.defaultHints { NS.addrFlags      = flags
                        , NS.addrSocketType = NS.Stream
                        }
   in liftIO $ head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)

openServer :: MonadIO m => NS.AddrInfo -> (Socket -> IO ()) -> m Socket
openServer NS.AddrInfo{..} setSocketOption = liftIO $ do
  sock <- NS.socket addrFamily addrSocketType addrProtocol
  setSocketOption sock
  NS.bind sock addrAddress
  NS.listen sock (max 2048 NS.maxListenQueue)
  return sock

openClient :: MonadIO m => NS.AddrInfo -> (Socket -> IO ()) -> m Socket
openClient NS.AddrInfo{..} setSocketOption = liftIO $ do
  sock <- NS.socket addrFamily addrSocketType addrProtocol
  setSocketOption sock
  NS.connect sock addrAddress
  return sock

close :: MonadIO m => Socket -> m ()
close s = liftIO $
  Ex.catch (Ex.finally (NS.shutdown s NS.ShutdownBoth)
                       (NS.close s))
           (\(_ :: Ex.SomeException) -> pure ())

-- | Graceful close the 'Socket' with printing errors if 'Ex.SomeException'
-- happened.
clean :: String -> Socket -> Either Ex.SomeException a -> IO ()
clean label lsock = \case
  Left e -> err e >> gracefulClose lsock
  Right _ -> gracefulClose lsock
  where
    err :: Ex.SomeException -> IO ()
    err e = IO.hPutStrLn IO.stderr (label ++ show e)
    gracefulClose :: MonadIO m => Socket -> m ()
    gracefulClose conn = liftIO $ NS.gracefulClose conn 5000

acceptConc
  :: (MonadBaseControl IO m, MonadIO m)
  => ((Socket, NS.SockAddr) -> m ())
  -> ((Either Ex.SomeException (), Socket) -> m ())
  -> Socket
  -> m a
acceptConc server release sock = forever $ do
  (conn, peer) <- liftIO $ NS.accept sock
  void $ L.forkFinally (server (conn, peer)) (\r -> release (r, conn))

connectSock
  :: MonadIO m
  => HostName
  -> ServiceName
  -> m (Socket, NS.SockAddr)
connectSock host port = do
  addr <- resolve host port False
  sock <- openClient addr setDefaultClientSO
  return (sock, NS.addrAddress addr)

gbracket :: MonadBaseControl IO m => IO a -> (a -> IO b) -> (a -> m c) -> m c
gbracket acquire release = liftBaseOp (Ex.bracket acquire release)
{-# INLINABLE gbracket #-}

pack :: Show a => a -> ByteString
pack = BSC.pack . show
{-# INLINE pack #-}
