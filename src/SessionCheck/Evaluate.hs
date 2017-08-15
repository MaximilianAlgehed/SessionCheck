{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Test.QuickCheck
import Data.Maybe

import SessionCheck.Types
import SessionCheck.Classes

data Status t = SSend t
              | Done
              | Skip
              | Step
              | Timeout
              | Bad String
              | Amb String
              deriving (Ord, Eq)

instance Show t => Show (Status t) where
  show (SSend t) = "Send: " ++ show t
  show Done     = "Done"
  show Skip     = "Skip"
  show Step     = "Step"
  show Timeout  = "Timeout"
  show (Bad e)  = "Bad: " ++ e
  show (Amb e)  = "Ambiguity: " ++ e

isError :: Status t -> Bool
isError s = case s of
  Bad _   -> True
  Amb _   -> True
  Timeout -> True
  _       -> False

isSkip :: Status t -> Bool
isSkip s = case s of
  Skip -> True
  _    -> False

maybeSend :: Status t -> TChan t -> IO ()
maybeSend st wc = case st of
  SSend t -> atomically $ writeTChan wc t
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

traceAccepts :: t -> Trace t -> Bool
traceAccepts t (Hide s _) = accepts s t

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
eval _  _ [] = return Done
eval _  0 _  = return (Bad $ "no progress")
eval imp fuel trs = do
  (trs', st) <- step imp trs
  let fuel' = if isSkip st then fuel - 1 else length trs'
  if isError st then
    return st
  else do
    maybeSend st (outputChan imp)
    eval imp fuel' trs'

step :: Implementation t -> [Trace t] -> IO ([Trace t], Status t)
step _ [] = return ([], Done)
step imp trs@((Hide s c):ts) = case s of
  Get p    -> do
    mt <- atomically $ tryPeekTChan (inputChan imp)
    case mt of
      Nothing -> return (trs, Skip)
      Just i  ->
        if test p i then
          if any (traceAccepts i) ts then
            return (trs, Amb $ "get " ++ name p) -- TODO better error here
          else do
            atomically $ readTChan (inputChan imp)
            return (ts ++ [hide $ c (fromJust (prj i))], Step)
        else
          return (ts ++ [Hide s c], Skip) 

  Send p   -> do
    a <- generate (satisfies p)
    if any (traceProduces (inj a)) ts then
      return (trs, Amb $ "send " ++ name p) -- TODO better error here
    else
      return (ts ++ [hide (c a)], SSend (inj a))

  Fork t   -> return (ts ++ [hide t, hide (c ())], Step)

  Stop     -> return (ts, Done)
  
  Return a -> return ((Hide (c a) (\_ -> Stop)):ts, Step)

  Bind s f -> return ((Hide s (\a -> f a >>= c)):ts, Step)
