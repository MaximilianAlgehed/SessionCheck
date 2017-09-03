{-# LANGUAGE GADTs, TypeOperators #-}
module SessionCheck.Evaluate where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.DeepSeq
import System.Timeout
import Test.QuickCheck
import Data.Maybe
import Data.List

import SessionCheck.Spec hiding (get)
import SessionCheck.Classes
import SessionCheck.Predicate
import SessionCheck.Backend
import SessionCheck.Types

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
  Async tr  -> accepts tr t
  Bind tr _ -> accepts tr t
  _         -> False

-- Check if a specification could have produced a message
canProduce :: Spec t a -> t -> Bool
canProduce tr t = case tr of
  Send p    -> test p t
  Async tr  -> canProduce tr t
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
  (s, w) <- runEvalM s imp Nothing eval
  kill imp (either id (const Ok) s)
  return (s, w)

runEvalM :: Spec t a
         -> Implementation t
         -> Maybe [LogEntry t]
         -> EvalM t b
         -> IO (Either (Status t) b, [LogEntry t])
runEvalM s imp log = runWriterT .
                     runExceptT .
                     flip runReaderT imp .
                     flip evalStateT (SS [] [hide s] log)

data SchedulerState t = SS { getting       :: [Thread t]
                           , sending       :: [Thread t]
                           , previousTrace :: Maybe [LogEntry t] }

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
  _      -> stepThread (t, Nothing, return ())

-- Returns: a thread, a possible previous entry, and an action to perform if progress is made
wakeThread :: Show t => EvalM t (Thread t, Maybe t, EvalM t ())
wakeThread = do
  ss <- get
  when (empty ss) (throwError $ Bad "Internal error")
  prg <- liftIO $ generate
    (frequency [(length (sending ss), return True), (length (getting ss), return False)])
  -- TODO:
  -- It is possible that this decision should be influenced by the
  -- current trace (if we have it)
  if (not . null $ sending ss) && ((null $ getting ss) || prg) then
    wakeSending 
  else
    wakeGetting
  where
    wakeGetting = do
      ss <- get
      case previousTrace ss of 
        Just (Input t : tr) -> do
          -- Naive solution for now, not expecting this to work well with concurrent traces
          g' <- liftIO $ generate $ shuffle (getting ss)
          modify $ \ss -> ss { getting = tail g' }
          return ( head g'
                 -- Might want to replace this by a look ahead instead
                 , if traceAccepts t (head g') then Just t else Nothing
                 , modify $ \ss -> ss { previousTrace = fmap tail $ previousTrace ss } )

        Just (InputViolates _ t : tr) -> do
          -- Naive solution for now, not expecting this to work well with concurrent traces
          g' <- liftIO $ generate $ shuffle (getting ss)
          modify $ \ss -> ss { getting = tail g' }
          return ( head g'
                 -- Might want to replace this by a look ahead instead
                 , if not (traceAccepts t (head g')) then Just t else Nothing
                 , modify $ \ss -> ss { previousTrace = fmap tail $ previousTrace ss } )

        Just tr -> error "Not yet implemented" -- TODO: Implement the look ahead

        Nothing -> do
          g' <- liftIO $ generate $ shuffle (getting ss)
          modify $ \ss -> ss { getting = tail g' }
          return ( head g'
                 , Nothing
                 , return ())

    wakeSending = do
      ss <- get
      case previousTrace ss of
        Just (Output t : tr) -> do
          -- Naive solution for now, not expecting this to work well with concurrent
          -- traces
          p' <- liftIO $ generate $ shuffle (sending ss)
          modify $ \ss -> ss { sending = tail p' }
          return ( head p'
                 -- Might want to replace this by a look ahead instead
                 , if traceProduces t (head p') then Just t else Nothing
                 , modify $ \ss -> ss { previousTrace = fmap tail $ previousTrace ss } )

        Just tr -> error "Not yet implemented" -- TODO: Implement the look ahead

        Nothing -> do
          p' <- liftIO $ generate $ shuffle (sending ss)
          modify $ \ss -> ss { sending = tail p' }
          return ( head p'
                 , Nothing
                 , return ())

eval :: Show t => EvalM t ()
eval = do
  e <- gets empty 
  unless e $ do
    step
    eval

-- Take a single step in evaluating the current set of threads
step :: Show t => EvalM t ()
step = do
  s <- gets sending
  -- When there are no sending threads we _need_ to get a message within a timeout
  when (null s) $ do
    imp <- ask
    g <- gets getting
    mi  <- liftIO $ peekLong imp
    case mi of
      Nothing -> do
        let names = "[" ++ intercalate ","  (map (\(Hide (Get p) _) -> name p) g) ++ "]"
        d <- liftIO $ isDead imp
        when d (do
          r <- liftIO $ readMVar (deadReason imp)
          throwError $ Timeout ("Timeout on gets " ++
                                names ++ " and channel is dead with reason " ++
                                show r))
        throwError $ Timeout $ "Timeout on gets: " ++ names
      _       -> return ()
  t <- wakeThread
  stepThread t

stepThread :: Show t => (Thread t, Maybe t, EvalM t ()) -> EvalM t ()
stepThread (t@(Hide s c), mprev, success) = do
  imp <- ask
  case s of
    Get p -> do
      mi <- liftIO $ peek imp
      ts <- gets getting
      case mi of
        Nothing -> scheduleThread t
        Just i  -> if test p i then do
                     tell [Input i]
                     when (any (traceAccepts i) ts) 
                          (throwError (Amb $ "get " ++ name p))
                     liftIO $ pop imp
                     scheduleThread (hide $ c (fromJust (prj i)))
                     success
                   else do
                     unless (any (traceAccepts i) ts) $ do
                       tell [InputViolates (name p) i]
                       throwError (Bad $ "get " ++ name p)
                     scheduleThread t
                     success

    Send p -> do
      a <- generateTimeoutShrink p mprev
      let msg = inj a
      ts <- gets sending
      if any (traceProduces msg) ts then
        throwError (Amb $ "send " ++ name p) -- TODO better error here
      else do
        d <- liftIO $ isDead imp
        when d (do
          r <- liftIO $ readMVar (deadReason imp)
          throwError $ Timeout ("trying to send \"" ++
                                name p ++ "\" but channel is dead with reason " ++
                                show r))
        void . liftIO . atomically $ writeTChan (outputChan imp) msg
        tell [Output (inj a)]
        scheduleThread (hide (c a))
        success
    
    Async t' -> mapM_ scheduleThread [hide t', hide (c ())]

    Stop -> return ()
    
    Return a -> scheduleThread (Hide (c a) (\_ -> Stop))

    Bind s f -> scheduleThread (Hide s (\a -> f a >>= c))

-- TODO: Implement
generateTimeoutShrink :: (a :< t, NFData a) => Predicate a -> Maybe t -> EvalM t a
generateTimeoutShrink p mprev = generateTimeout p

generateTimeout :: NFData a => Predicate a -> EvalM t a
generateTimeout p = do
  ma <- liftIO $ timeout (10^6) $ do
    a <- generate (satisfies p)
    a `deepseq` return a
  case ma of
    Nothing -> throwError . Timeout $ "Failed to satisfy: " ++ name p
    Just a  -> return a
