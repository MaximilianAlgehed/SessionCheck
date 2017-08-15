{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Control.Concurrent.Chan
import Test.QuickCheck
import Data.Maybe

import SessionCheck.Types
import SessionCheck.Classes

data Status t = SSend t
              | Done
              | Skip
              | Step
              | Bad String
              | Amb String
              deriving (Ord, Eq)

instance Show t => Show (Status t) where
  show (SSend t) = "Send: " ++ show t
  show Done     = "Done"
  show Skip     = "Skip"
  show Step     = "Step"
  show (Bad e)  = "Bad: " ++ e
  show (Amb e)  = "Ambiguity: " ++ e

isError :: Status t -> Bool
isError s = case s of
  Bad _ -> True
  Amb _ -> True
  _     -> False

isSkip :: Status t -> Bool
isSkip s = case s of
  Skip -> True
  _    -> False

maybeSend :: Status t -> Chan t -> IO ()
maybeSend st wc = case st of
  SSend t -> writeChan wc t
  _      -> return ()

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

data Implementation t = CC { outputChan :: Chan t
                           , inputChan  :: Chan t
                           , kill       :: IO () }

-- Rewrite this to use the channels directly instead of the lazy list as the
-- lazy list will block on stepping `Get`
evaluate :: Show t => Implementation t -> Spec t a -> IO ()
evaluate cc s = do
  ts <- getChanContents (inputChan cc) 
  s  <- eval (outputChan cc) ts 1 [hide s]
  kill cc
  print s

eval :: Show t => Chan t -> [t] -> Int -> [Trace t] -> IO (Status t)
eval _ _  _ [] = return Done
eval _ _  0 _  = return (Bad $ "no progress")
eval wc ts fuel trs = do
  (ts', trs', st) <- step ts trs
  let fuel' = if isSkip st then fuel - 1 else length trs'
  if isError st then
    return st
  else do
    maybeSend st wc
    eval wc ts' fuel' trs'

step :: [t] -> [Trace t] -> IO ([t], [Trace t], Status t)
step ins [] = return (ins, [], Done)
step ins trs@((Hide s c):ts) = case s of
  Get p    ->
    -- Add timeout here when we change to use the chan directly
    case ins of
      []     -> return (ins, trs, Bad $ "Ran out of inputs at: get " ++ name p)
      (i:is) ->
        if test p i then
          if any (traceAccepts i) ts then
            return (ins, trs, Amb $ "get " ++ name p) -- TODO better error here
          else
            return (is, ts ++ [hide $ c (fromJust (prj i))], Step)
        else
          return (i:is, ts ++ [Hide s c], Skip) 

  Send p   -> do
    a <- generate (satisfies p)
    if any (traceProduces (inj a)) ts then
      return (ins, trs, Amb $ "send " ++ name p) -- TODO better error here
    else
      return (ins, ts ++ [hide (c a)], SSend (inj a))

  Fork t   -> return (ins, ts ++ [hide t, hide (c ())], Step)

  Stop     -> return (ins, ts, Done)
  
  Return a -> return (ins, (Hide (c a) (\_ -> Stop)):ts, Step)

  Bind s f -> return (ins, (Hide s (\a -> f a >>= c)):ts, Step)
