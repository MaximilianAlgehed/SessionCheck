{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad.Writer
import Test.QuickCheck
import Data.Maybe
import Data.IORef

import SessionCheck.Spec
import SessionCheck.Classes
import SessionCheck.Predicate
import SessionCheck.Backend

-- The status of taking a single step in the execution of a protocol
data Status t = Sent t
              | Got  t
              | Done
              | Skip
              | GotBad String t
              | Step
              | Bad String
              | Amb String
              | Timeout

instance Show t => Show (Status t) where
  show (Sent t)     = "Sent: " ++ show t
  show (Got t)      = "Got: " ++ show t
  show (GotBad p t) = "GotBad: " ++ p ++ " " ++ show t
  show Done         = "Done"
  show Skip         = "Skip"
  show Step         = "Step"
  show (Bad e)      = "Bad: " ++ e
  show (Amb e)      = "Ambiguity: " ++ e
  show Timeout      = "Timeout"

-- Check if a status represents an error
isError :: Status t -> Bool
isError s = case s of
  Bad _   -> True
  Amb _   -> True
  Timeout -> True
  _       -> False

-- Check if a status is a `Skip`
isSkip :: Status t -> Bool
isSkip s = case s of
  Skip -> True
  GotBad _ t -> True
  _    -> False

-- Send a value on a `TChan` if the status is a `Sent`
maybeSend :: Status t -> Implementation t -> IO (Status t)
maybeSend st imp = case st of
  Sent t -> do
    d <- readIORef (dead imp)
    if d then
      return Timeout
    else do
      atomically $ writeTChan (outputChan imp) t
      return st
  _      -> return Skip

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

-- Lift `accepts` to a `Thread`
traceAccepts :: t -> Thread t -> Bool
traceAccepts t (Hide s _) = accepts s t

-- Lift `canProduce` to a `Thread`
traceProduces :: t -> Thread t -> Bool
traceProduces t (Hide s _) = canProduce s t

data LogEntry t = Input t
                | InputViolates String t
                | Output t deriving (Ord, Eq, Show)

maybeLog :: Monad m => Status t -> Int -> WriterT [LogEntry t] m ()
maybeLog s f = case s of
  Got t  -> tell [Input t]
  Sent t -> tell [Output t]
  GotBad p t -> if f == 0 then tell [InputViolates p t] else return ()
  _      -> return ()

printTrace :: Show t => [LogEntry t] -> String
printTrace = unlines . map show

-- TODO: Change this to return counterexample if found
evaluate :: Show t
         => Implementation t
         -> Spec t a
         -> IO (Status t, [LogEntry t])
evaluate imp s = do
  (s, w) <- runWriterT $ eval imp 1 [hide s]
  kill imp
  return (s, w)

-- TODO:
--  *Use error monad transformer instead perhaps?
--  *Use writer monad transformer to track send and get messages
eval :: Show t
     => Implementation t
     -> Int
     -> [Thread t]
     -> WriterT [LogEntry t] IO (Status t)
eval _ _ [] = return Done -- We are finished
eval _ 0 _  = return (Bad $ "no progress") -- No thread made any progress
eval imp fuel trs = do
  (trs', st) <- liftIO $ step imp trs
  let fuel' = if isSkip st then fuel - 1 else length trs'
  maybeLog st fuel'
  if isError st then -- The protocol test failed
    return st
  else do
    -- If the result of the step was a send, send that message
    st <- liftIO $ maybeSend st imp
    if not (isError st) then
      do -- Ensure that the scheduling is not round robing
         trs' <- liftIO $ generate (shuffle trs') 
         -- Loop
         eval imp fuel' trs'
    else
      return st

-- Take a single step in evaluating the current set of threads
step :: Implementation t -> [Thread t] -> IO ([Thread t], Status t)
step _ [] = return ([], Done)
step imp trs@((Hide s c):ts) = case s of
  Get p    -> do
    mi <- peek imp
    case mi of
      Nothing -> return (trs, Timeout)
      Just i  -> if test p i then
                   if any (traceAccepts i) ts then
                     return (trs, Amb $ "get " ++ name p)
                   else do
                     pop imp
                     return (ts ++ [hide $ c (fromJust (prj i))], Got i)
                 else
                   return (ts ++ [Hide s c], GotBad (name p) i)

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
