{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module PingPong where

import Data.List
import Control.Monad

import SessionCheck
import SessionCheck.Backend.Erlang

pingPong :: (Atom :< t, (Atom, Atom) :< t) => [Atom] -> Spec t ()
pingPong ps = do
  op <- send $ if null ps
               then anyOf [ is (atom "start")
                          , is (atom "stop")]
               else anyOf [ is (atom "start")
                          , is (atom "ping")
                          , is (atom "kill")
                          , is (atom "stop")]
  case op of
    Atom "start" -> do
      p <- send anything
      pingPong (p : ps)
    Atom "ping"  -> do
      p <- choose ps
      async $ get (is (atom "pong", p))
      pingPong ps
    Atom "kill"  -> do
      p <- choose ps
      pingPong (ps \\ [p])
    Atom "stop"  -> stop

main :: IO ()
main = do
  checkCoherence (pingPong [])
