{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Queue where

import SessionCheck
import SessionCheck.Backend.Erlang

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail = drop 1

queue :: (Int :< t, Maybe Int :< t, Atom :< t) => [Int] -> Spec t ()
queue q = do
  op <- choose $ map atom ["enqueue", "pop", "peek", "stop"]
  case op of
    Atom "enqueue" -> do
      i <- send anyInt
      queue (q ++ [i])
    Atom "pop" -> do
      get (is (safeHead q))
      queue (safeTail q)
    Atom "peek" -> do
      get (is (safeHead q))
      queue q
    Atom "stop" -> stop

main :: IO ()
main = do
  checkCoherence (queue [])
