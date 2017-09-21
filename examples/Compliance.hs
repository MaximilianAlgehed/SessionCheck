{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , ScopedTypeVariables #-}
module Compliance where

import Control.Monad

import SessionCheck

coherent1 :: Int :< t => Spec t ()
coherent1 = void $ do
  p <- send posInt
  q <- send negInt
  get (inRange q p)

version0 :: (Bool :< t, Int :< t) => Spec t Int
version0 = do
  vnum :: Int <- branch [0, 1]
  when (vnum /= 0) $ do
    send $ is False
    stop
  send $ is True
  send (is (-42 :: Int))
  get (is (-42))

version1 :: (Bool :< t, Int :< t) => Spec t Int
version1 = do
  vnum :: Int <- send (is 1)
  otherParty <- get $ anything
  unless otherParty stop
  if vnum == 0 then
    do
      send (is (-42 :: Int))
      get (is (-42))
  else
    do
      send (is (42 :: Int))
      get (is 42)

main :: IO ()
main = checkCompatibility version0 (dual version1)
