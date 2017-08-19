{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.DeepSeq
import System.Timeout
import Test.QuickCheck
import Data.Maybe

import SessionCheck.Spec hiding (get)
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
              | Timeout String

instance Show t => Show (Status t) where
  show (Sent t)     = "Sent: " ++ show t
  show (Got t)      = "Got: " ++ show t
  show (GotBad p t) = "GotBad: " ++ p ++ " " ++ show t
  show Done         = "Done"
  show Skip         = "Skip"
  show Step         = "Step"
  show (Bad e)      = "Bad: " ++ e
  show (Amb e)      = "Ambiguity: " ++ e
  show (Timeout e)  = "Timeout: " ++ e

-- Check if a status represents an error
isError :: Status t -> Bool
isError s = case s of
  Bad _     -> True
  Amb _     -> True
  Timeout _ -> True
  _         -> False

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

maybeLog :: Status t -> EvalM t ()
maybeLog s = case s of
  Got t  -> tell [Input t]
  Sent t -> tell [Output t]
  _      -> return ()

printTrace :: Show t => [LogEntry t] -> String
printTrace = unlines . map show

evaluate :: Show t
         => Implementation t
         -> Spec t a
         -> IO (Either (Status t) (), [LogEntry t])
evaluate imp s = do
  (s, w) <- runEvalM s imp eval
  kill imp
  return (s, w)

runEvalM :: Spec t a
         -> Implementation t
         -> EvalM t b
         -> IO (Either (Status t) b, [LogEntry t])
runEvalM s imp = runWriterT .
                 runExceptT .
                 flip runReaderT imp .
                 flip evalStateT (SS [] [hide s])

data SchedulerState t = SS { getting :: [Thread t]
                           , sending :: [Thread t] }

empty :: SchedulerState t -> Bool
empty ss = null (getting ss) && null (sending ss)

type EvalM t a = StateT (SchedulerState t)
                  (ReaderT (Implementation t)
                    (ExceptT (Status t)
                      (WriterT [LogEntry t] IO))) a

scheduleThread :: Show t => Thread t -> EvalM t ()
scheduleThread t@(Hide s _) = case s of
    Get _  -> modify $ \ss -> ss { getting = t : getting ss }
    Send _ -> modify $ \ss -> ss { sending = t : sending ss }
    _      -> stepThread t

wakeThread :: Show t => EvalM t (Thread t)
wakeThread = do
  ss <- get
  when (empty ss) (throwError $ Amb "Internal error")
  prg <- liftIO $ generate arbitrary
  if (not . null $ sending ss) && ((null $ getting ss) || prg) then
    wakeSending 
  else
    wakeGetting
  where
    wakeGetting = do
      ss <- get
      g' <- liftIO $ generate $ shuffle (getting ss)
      modify $ \ss -> ss { getting = tail g' }
      return (head g')
    wakeSending = do
      ss <- get
      p' <- liftIO $ generate $ shuffle (sending ss)
      modify $ \ss -> ss { sending = tail p' }
      return (head p')

eval :: Show t => EvalM t ()
eval = do
  e <- gets empty 
  unless e $ do
    step
    eval

-- Take a single step in evaluating the current set of threads
step :: Show t => EvalM t ()
step = do
  t <- wakeThread
  stepThread t

stepThread :: Show t => Thread t -> EvalM t ()
stepThread (Hide s c) = do
  imp <- ask
  case s of
    Get p -> do
      mi <- liftIO $ peek imp
      ts <- gets getting
      case mi of
        Nothing -> throwError $ Timeout $ "get " ++ name p 
        Just i  -> if test p i then do
                     tell [Input i]
                     when (any (traceAccepts i) ts) 
                          (throwError (Amb $ "get " ++ name p))
                     liftIO $ pop imp
                     scheduleThread (hide $ c (fromJust (prj i)))
                   else do
                     unless (any (traceAccepts i) ts) $ do
                       tell [InputViolates (name p) i]
                       throwError (Bad $ "get " ++ name p)
                     scheduleThread (Hide s c)

    Send p   -> do
      a <- generateTimeout p
      let msg = inj a
      ts <- gets sending
      if any (traceProduces msg) ts then
        throwError (Amb $ "send " ++ name p) -- TODO better error here
      else do
        d <- liftIO $ isDead imp
        when d (throwError $ Timeout ("send " ++ name p))
        void . liftIO . atomically $ writeTChan (outputChan imp) msg
        tell [Output (inj a)]
        scheduleThread (hide (c a))
    
    Fork t   -> do
      mapM scheduleThread [hide t, hide (c ())]
      return ()

    Stop     -> return ()
    
    {- Change these two to run to conclusion! -}
    Return a -> scheduleThread (Hide (c a) (\_ -> Stop))

    Bind s f -> scheduleThread (Hide s (\a -> f a >>= c))

generateTimeout :: NFData a => Predicate a -> EvalM t a
generateTimeout p = do
  ma <- liftIO $ timeout (10^6) $ do
    a <- generate (satisfies p)
    a `deepseq` return a
  case ma of
    Nothing -> throwError . Timeout $ "Failed to satisfy: " ++ name p
    Just a  -> return a
