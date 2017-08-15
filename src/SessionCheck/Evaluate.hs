{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Test.QuickCheck
import Data.Maybe

import SessionCheck.Spec
import SessionCheck.Classes
import SessionCheck.Predicate

data Status t = Sent t
              | Got  t
              | Done
              | Skip
              | Step
              | Bad String
              | Amb String
              deriving (Ord, Eq)

instance Show t => Show (Status t) where
  show (Sent t) = "Sent: " ++ show t
  show (Got t)  = "Got: " ++ show t
  show Done     = "Done"
  show Skip     = "Skip"
  show Step     = "Step"
  show (Bad e)  = "Bad: " ++ e
  show (Amb e)  = "Ambiguity: " ++ e

isError :: Status t -> Bool
isError s = case s of
  Bad _   -> True
  Amb _   -> True
  _       -> False

isSkip :: Status t -> Bool
isSkip s = case s of
  Skip -> True
  _    -> False

maybeSend :: Status t -> TChan t -> IO ()
maybeSend st wc = case st of
  Sent t -> atomically $ writeTChan wc t
  _       -> return ()

data Trace t where
  Hide :: Spec t a -> (a -> Spec t b) -> Trace t

hide :: Spec t a -> Trace t
hide s = Hide s (\_-> Stop)

-- Check if a specification can accept a message
accepts :: Spec t a -> t -> Bool
accepts tr t = case tr of
  Get p     -> test p t
  Fork tr   -> accepts tr t
  Bind tr _ -> accepts tr t
  _         -> False

-- Check if a specification could have produces a message
canProduce :: Spec t a -> t -> Bool
canProduce tr t = case tr of
  Send p    -> test p t
  Fork tr   -> canProduce tr t
  Bind tr _ -> canProduce tr t
  _         -> False

-- Lift `accepts` to a `Trace`
traceAccepts :: t -> Trace t -> Bool
traceAccepts t (Hide s _) = accepts s t

-- Lift `canProduce` to a `Trace`
traceProduces :: t -> Trace t -> Bool
traceProduces t (Hide s _) = canProduce s t

data Implementation t = CC { outputChan :: TChan t
                           , inputChan  :: TChan t
                           , kill       :: IO () }

evaluate :: Show t => Implementation t -> Spec t a -> IO ()
evaluate imp s = do
  s  <- eval imp 1 [hide s]
  kill imp 
  print s

eval :: Show t => Implementation t -> Int -> [Trace t] -> IO (Status t)
eval _  _ [] = return Done -- We are finished
eval _  0 _  = return (Bad $ "no progress") -- No thread made any progress
eval imp fuel trs = do
  (trs', st) <- step imp trs
  let fuel' = if isSkip st then fuel - 1 else length trs'
  if isError st then -- The protocol test failed
    return st
  else do
    -- If the result of the step was a send, send that message
    maybeSend st (outputChan imp)
    -- Ensure that the scheduling is not round robing
    trs' <- generate (shuffle trs') 
    -- Loop
    eval imp fuel' trs'

step :: Implementation t -> [Trace t] -> IO ([Trace t], Status t)
step _ [] = return ([], Done)
step imp trs@((Hide s c):ts) = case s of
  Get p    -> do
    i <- atomically $ peekTChan (inputChan imp)
    if test p i then
      if any (traceAccepts i) ts then
        return (trs, Amb $ "get " ++ name p) -- TODO better error here
      else do
        atomically $ readTChan (inputChan imp)
        return (ts ++ [hide $ c (fromJust (prj i))], Got i)
    else
      return (ts ++ [Hide s c], Skip) 

  Send p   -> do
    a <- generate (satisfies p)
    if any (traceProduces (inj a)) ts then
      return (trs, Amb $ "send " ++ name p) -- TODO better error here
    else
      return (ts ++ [hide (c a)], Sent (inj a))
  
  Fork t   -> return (ts ++ [hide t, hide (c ())], Step)

  Stop     -> return (ts, Done)
  
  Return a -> return ((Hide (c a) (\_ -> Stop)):ts, Step)

  Bind s f -> return ((Hide s (\a -> f a >>= c)):ts, Step)
