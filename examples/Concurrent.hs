{-# LANGUAGE TypeOperators
           , FlexibleContexts #-}
module Concurrent where

import SessionCheck
import SessionCheck.Backend.Erlang

protocol :: Int :< t => Spec t Int
protocol = do
  async $ do
    i <- branch [1, (2 :: Int)]
    send (is i)
  i <- choose [4, 5]
  get (is i)

main :: IO ()
main = do
  checkCoherence protocol
  erlangMain "concurrent:main" protocol
