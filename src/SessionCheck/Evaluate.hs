{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Test.QuickCheck
import Data.Maybe

import SessionCheck.Types
import SessionCheck.Classes

data Status t = Res t
              | Done
              | Skip
              | Step
              | Bad String
              | Amb String
              deriving (Ord, Eq)

instance Show t => Show (Status t) where
  show (Res t) = "Result: " ++ show t
  show Done    = "Done"
  show Skip    = "Skip"
  show Step    = "Step"
  show (Bad e) = "Bad: " ++ e
  show (Amb e) = "Ambiguity: " ++ e

isError :: Status t -> Bool
isError s = case s of
  Bad _ -> True
  Amb _ -> True
  _     -> False

isSkip :: Status t -> Bool
isSkip s = case s of
  Skip -> True
  _    -> False

data Trace t where
  Hide :: Spec t a -> (a -> Spec t b) -> Trace t

hide :: Spec t a -> Trace t
hide s = Hide s (\_-> Stop)

-- Check if a specification can accept a message
accepts :: Spec t a -> t -> Bool
accepts tr t = case tr of
  Get p     -> test p t
  Both l r  -> accepts l t || accepts l t
  Bind tr _ -> accepts tr t
  _         -> False

-- Check if a specification could have produces a message
canProduce :: Spec t a -> t -> Bool
canProduce tr t = case tr of
  Send p    -> test p t
  Both l r  -> canProduce l t || canProduce r t
  Bind tr _ -> canProduce tr t
  _         -> False

traceAccepts :: t -> Trace t -> Bool
traceAccepts t (Hide s _) = accepts s t

traceProduces :: t -> Trace t -> Bool
traceProduces t (Hide s _) = canProduce s t

step :: [t] -> [Trace t] -> IO ([t], [Trace t], Status t)
step ins [] = return (ins, [], Done)
step ins trs@((Hide s c):ts) = case s of
  Get p    ->
    case ins of
      []     -> return (ins, trs, Bad $ "Ran out of inputs at: get " ++ name p)
      (i:is) ->
        if test p i then
          if any (traceAccepts i) ts then
            return (ins, trs, Amb $ "get " ++ name p) -- TODO better error here
          else
            return (is, ts ++ [hide $ c (fromJust (prj i))], Res i)
        else
          return (i:is, ts ++ [Hide s c], Skip) 

  Send p   -> do
    a <- generate (satisfies p)
    if any (traceProduces (inj a)) ts then
      return (ins, trs, Amb $ "send " ++ name p) -- TODO better error here
    else
      return (ins, ts ++ [hide (c a)], Res (inj a))

  Both l r -> return (ins, ts ++ [hide l, hide r, hide (c ())], Step)

  Stop     -> return (ins, ts, Done)
  
  Return a -> return (ins, (Hide (c a) (\_ -> Stop)):ts, Step)

  Bind s f -> return (ins, (Hide s (\a -> f a >>= c)):ts, Step)

evaluate :: Show t => [t] -> Spec t a -> IO ()
evaluate ts s = do
  s <- eval ts 1 [hide s]
  print s

eval :: Show t => [t] -> Int -> [Trace t] -> IO (Status t)
eval _  _ [] = return Done
eval _  0 _  = return (Bad $ "no progress")
eval ts fuel trs = do
  (ts', trs', st) <- step ts trs
  let fuel' = if isSkip st then fuel - 1 else length trs'
  if isError st then
    return st
  else
    eval ts' fuel' trs'
