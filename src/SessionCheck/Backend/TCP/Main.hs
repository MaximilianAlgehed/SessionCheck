{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.TCP.Main ( tcpMain )where

import SessionCheck.Spec (Spec)
import SessionCheck.Backend
import SessionCheck.Backend.TCP.Types
import SessionCheck.Types
import SessionCheck.Test

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, w2c)
import Network.Simple.TCP
import qualified Network.Socket as N
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import System.Timeout
import System.Process

-- Convert a String to an effing ByteString
pack :: String -> BS.ByteString
pack = BS.pack . map c2w

-- Convert an effing ByteString to a String
unpack :: BS.ByteString -> String
unpack = map w2c . BS.unpack

-- Write a TCPMessage to a socket, appends a <CR><LF>
-- to the message
write :: Socket -> TCPMessage -> IO ()
write s = send s . pack . (++"\r\n") . unTCPMessage

-- Read a message from a socket, works by incrementally reading individual
-- bytes until it encounters a <CR><LF> pair
read :: Socket -> IO (Maybe TCPMessage)
read s = go BS.empty >>= return . fmap (TCPMessage . unpack)
  where
    go bs = do
      mb <- recv s 1
      case mb of
        Nothing -> return Nothing
        Just b  ->
          if BS.elem (c2w '\r') b then do
            mb' <- recv s 1
            case mb' of
              Nothing -> return Nothing
              Just b' ->
                if BS.elem (c2w '\n') b' then
                  return $ Just bs
                else
                  go (BS.concat [bs, b, b'])
          else
            go (BS.append bs b)

data Options = Options { program :: String
                       , port    :: String
                       , role    :: ProtocolRole
                       , socket  :: Maybe Socket }

runFun :: Options
       -> Implementation TCPMessage
       -> IO ()
runFun opts imp = do
  case role opts of
    Server -> do
      let Just s = socket opts
      ph <- spawnCommand $ program opts ++ " > /dev/null"
      (sock, sockAdr) <- N.accept s
      readTid <- forkIO $ readThread sock
      loop sock
      killThread readTid
      terminateProcess ph
    Client -> do
      (s, a) <- connectSock "127.0.0.1" (port opts)
      readTid <- forkIO $ readThread s
      loop s 
      killThread readTid
      closeSock s
  putMVar (done imp) ()
  where
    -- Do the reading
    readThread sock = do
      m <- read sock
      maybe (kill imp (Timeout "Process died")) (atomically . writeTChan (inputChan imp)) m
      readThread sock

    -- Do the writing
    loop sock = do
      d <- isDead imp
      unless d $ do
        toTCP <- timeout 1000 $ atomically $ readTChan (outputChan imp)
        maybe (return ())
              (write sock) 
              toTCP
        loop sock 

tcpMain :: ProtocolRole
        -> String            -- Program to run
        -> Int               -- Port number
        -> Spec TCPMessage a -- Specification
        -> IO ()
tcpMain r prog prt spec = do
  (sock, ph) <- case r of
                   Server -> do
                     (s, a) <- bindSock (Host "127.0.0.1") (show prt)
                     N.listen s 1
                     return (Just s, Nothing)
                   Client -> do
                    ph <- spawnCommand $ prog ++ " > /dev/null"
                    threadDelay 1000000
                    return (Nothing, Just ph)
  let opts = Options { program = prog
                     , port = show prt
                     , role = r
                     , socket = sock }
  imp <- clean 
  sessionCheck (imp { run = runFun opts imp }) spec
  maybe (return ()) closeSock sock
  maybe (return ()) terminateProcess ph
