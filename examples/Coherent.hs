{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Coherent where

import Control.Monad

import SessionCheck

coherent1 :: Int :< t => Spec t ()
coherent1 = void $ do
  p <- send posInt
  q <- send negInt
  get (inRange q p)

incoherent1 :: Int :< t => Spec t ()
incoherent1 = void $ do
  p <- send posInt
  q <- send negInt
  get (inRange p q)

incoherent2 :: Int :< t => Spec t ()
incoherent2 = void $ do
  fork $ send anyInt
  send anyInt

incoherent3 :: () :< t => Spec t ()
incoherent3 = void $ do
  fork $ get (is ())
  send (is ())

main :: IO ()
main = do
  checkCoherence coherent1
  checkCoherence incoherent1
  checkCoherence incoherent2
  checkCoherence incoherent3
