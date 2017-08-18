{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Coherent where

import SessionCheck

incoherent :: Int :< t => Spec t ()
incoherent = do
  p <- send posInt
  q <- send negInt
  get (inRange p q)
  return ()

main :: IO ()
main = checkCoherence incoherent
