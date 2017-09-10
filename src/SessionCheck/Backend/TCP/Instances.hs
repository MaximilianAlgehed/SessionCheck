{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module SessionCheck.Backend.TCP.Instances where

import SessionCheck.Backend.TCP.Types
import SessionCheck.Classes

instance [Char] :< TCPMessage where
  inj = TCPMessage
  prj = Just . unTCPMessage
