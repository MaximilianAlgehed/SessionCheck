{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.TCP.Types where

data ProtocolRole = Client | Server deriving (Ord, Eq, Show)

newtype TCPMessage = TCPMessage { unTCPMessage :: String }

instance Show TCPMessage where
  show = unTCPMessage
