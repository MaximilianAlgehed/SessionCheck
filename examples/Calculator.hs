{-# LANGUAGE TypeOperators
           , FlexibleContexts #-}
module Calculator where

import Control.Monad

import SessionCheck
import SessionCheck.Backend.Erlang

calculator :: (String :< t, Int :< t) => Spec t ()
calculator = do
  op <- choose ["mul", "div", "stop"]
  case op of
    "mul" -> do
      i <- send anyInt
      j <- send anyInt
      get $ is (i * j)
    "div" -> do
      i <- send nonNegInt
      j <- send posInt
      get $ is (i `div` j)
    "stop" -> stop
  calculator

main :: IO ()
main = do
  checkCoherence calculator
  erlangMain "calculator:main" calculator 
