{-# LANGUAGE GADTs #-}
module SessionCheck.Evaluate where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Test.QuickCheck
import Data.Maybe
import Data.IORef

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

data SchedulerState t = SS { getting     :: [Thread t]
                           , progressing :: [Thread t] }

empty :: SchedulerState t -> Bool
empty ss = null (getting ss) && null (progressing ss)

type EvalM t a = StateT (SchedulerState t)
                  (ReaderT (Implementation t)
                    (ExceptT (Status t)
                      (WriterT [LogEntry t] IO))) a

scheduleThread :: Thread t -> EvalM t ()
scheduleThread t@(Hide s _) =
  modify $ \ss -> case s of
    Get _ -> ss { getting     = t : getting ss }
    _     -> ss { progressing = t : progressing ss}

wakeThread :: EvalM t (Thread t)
wakeThread = do
  ss <- get
  when (empty ss) (throwError $ Timeout)
  prg <- liftIO $ generate arbitrary
  if (not . null $ progressing ss) && ((null $ getting ss) || prg) then
    wakeProgressing
  else
    wakeGetting
  where
    wakeGetting = do
      ss <- get
      g' <- liftIO $ generate $ shuffle (getting ss)
      modify $ \ss -> ss { getting = tail g' }
      return (head g')
    wakeProgressing = do
      ss <- get
      p' <- liftIO $ generate $ shuffle (progressing ss)
      modify $ \ss -> ss { progressing = tail p' }
      return (head p')

eval :: EvalM t ()
eval = do
  e <- gets empty 
  unless e $ do
    step
    eval

-- Take a single step in evaluating the current set of threads
step :: EvalM t ()
step = do
  t   <- wakeThread
  stepThread t
  where
  stepThread (Hide s c) = do
    imp <- ask
    case s of
      Get p    -> do
        mi <- liftIO $ peek imp
        ts <- gets getting
        case mi of
          Nothing -> throwError Timeout 
          Just i  -> if test p i then
                       if any (traceAccepts i) ts then
                         throwError (Amb $ "get " ++ name p)
                       else do
                         liftIO $ pop imp
                         scheduleThread (hide $ c (fromJust (prj i)))
                         tell [Input i]
                     else do
                       unless (any (traceAccepts i) ts) $ do
                         tell [InputViolates (name p) i]
                         throwError (Bad $ "get " ++ name p)
                       scheduleThread (Hide s c)

      Send p   -> do
        a <- liftIO $ generate (satisfies p)
        ts <- gets progressing
        if any (traceProduces (inj a)) ts then
          throwError (Amb $ "send " ++ name p) -- TODO better error here
        else do
          scheduleThread (hide (c a))
          d <- liftIO $ readIORef (dead imp)
          when d (throwError Timeout)
          void . liftIO . atomically $ writeTChan (outputChan imp) (inj a)
          tell [Output (inj a)]
      
      Fork t   -> do
        mapM scheduleThread [hide t, hide (c ())]
        return ()

      Stop     -> return ()
      
      {- Change these two to run to conclusion! -}
      Return a -> stepThread (Hide (c a) (\_ -> Stop))

      Bind s f -> stepThread (Hide s (\a -> f a >>= c))
