{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.TCP.Main ( tcpMain )where

import SessionCheck.Spec (Spec)
import SessionCheck.Backend.TCP.Types

import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w, w2c)
import Network.Simple.TCP

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

-- Read a message from a socket, incrementally reads individual
-- bytes until it encounters a <CR><LF> pair
read :: Socket -> IO (Maybe TCPMessage)
read s = go BS.empty >>= return . fmap (TCPMessage . reverse . unpack)
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

-- TODO: Implement
tcpMain :: String            -- Program to run
        -> Int               -- Port number
        -> Spec TCPMessage a -- Specification
        -> IO ()
tcpMain prog port spec = return ()
