{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.TCP.Main ( tcpMain )where

import SessionCheck.Spec
import SessionCheck.Backend.TCP.Types

-- TODO: Implement
tcpMain :: String            -- Program to run
        -> Int               -- Port number
        -> Spec TCPMessage a -- Specification
        -> IO ()
tcpMain prog port spec = return ()
