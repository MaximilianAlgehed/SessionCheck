{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Coherent where

import Control.Monad

import SessionCheck

coherent1 :: Int :< t => Spec t ()
coherent1 = void $ do
  p <- send posInt
  q <- send negInt
  get (inRange q p)

coherent2 :: () :< t => Spec t ()
coherent2 = void $ do
  interleave $ get (is ())
  send (is ())

incoherent1 :: Int :< t => Spec t ()
incoherent1 = void $ do
  p <- send posInt
  q <- send negInt
  get (inRange p q)

incoherent2 :: Int :< t => Spec t ()
incoherent2 = void $ do
  interleave $ send anyInt
  send anyInt

main :: IO ()
main = do
  putStrLn "Checking that \"coherent1\" is coherent"
  checkCoherence coherent1
  putStrLn "\nChecking that \"coherent2\" is coherent"
  checkCoherence coherent2
  putStrLn "\nChecking that \"incoherent1\" is coherent"
  checkCoherence incoherent1
  putStrLn "\nChecking that \"incoherent2\" is coherent"
  checkCoherence incoherent2
