module Concurrent where

import SessionCheck
import SessionCheck.Backend.Erlang

protocol :: Spec ErlType Int
protocol = do
  fork $ do
    i <- branch [1, (2 :: Int)]
    send (is i)
  i <- choose [4,5]
  get (is i)

main :: IO ()
main = erlangMain "concurrent:main" protocol
