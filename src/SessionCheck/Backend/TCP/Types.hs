{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.TCP.Types where

newtype TCPMessage = TCPMessage { unTCPMessage :: String }

instance Show TCPMessage where
  show = unTCPMessage
