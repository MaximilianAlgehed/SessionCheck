module Calculator where

import Control.Monad

import SessionCheck

calculator :: (String :< t, Int :< t) => Spec t ()
calculator = do
  op <- choose ["mul", "div"]
  case op of
    "mul" -> do
      i <- send anyInt
      j <- send anyInt
      get (i * j)
    "div" -> do
      i <- send anyInt
      j <- send anyInt
      get (i `div` j)
  calculator
