module Calculator where

import Control.Monad

import SessionCheck

calculator :: (String :< t, Int :< t) => Spec t ()
calculator = do
  op <- choose ["mul", "div"]
  return ()
