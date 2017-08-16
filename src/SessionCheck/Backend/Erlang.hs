module SessionCheck.Backend.Erlang where

import Foreign.Erlang
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Timeout
import System.Process

import SessionCheck.Backend
import SessionCheck.Spec
import SessionCheck.Test

data Options =
  Options { targetModule   :: String
          , targetFunction :: String
          , erlangNode     :: String
          , self           :: Self }

-- Should only be called once
newSession :: String -- ^ Target module
           -> String -- ^ Target function
           -> String -- ^ Target node
           -> IO Options
newSession mod fun node = do
  self <- createSelf "haskell@localhost"
  return $ Options mod fun node self

-- Generate an `Implementation` from the options
erlang :: Options -> IO (Implementation ErlType)
erlang opts = do
  readChan  <- atomically $ newTChan
  writeChan <- atomically $ newTChan
  isDead    <- newIORef False
  return $ Imp { outputChan = readChan
               , inputChan  = writeChan
               , dead       = isDead 
               , run        = runFun readChan writeChan isDead opts }

runFun :: TChan ErlType
       -> TChan ErlType 
       -> IORef Bool
       -> Options
       -> IO ()
runFun readChan writeChan dead opts = do
  mbox <- createMBox (self opts)
  rpcCall mbox (Short erl) (targetModule opts) (targetFunction opts) []
  mboxSend mbox (Short erl) (Right "p") (mboxSelf mbox)
  loop mbox
  pid <- rpcCall mbox (Short erl) "erlang" "whereis" [ErlAtom "p"]
  void $ rpcCall mbox (Short erl) "erlang" "exit" [pid, ErlAtom "ok"] 
  where
    erl = erlangNode opts
    loop mbox = do
      d <- readIORef dead
      unless d $ do
        toErlang <- timeout 1000 $ atomically $ readTChan readChan
        maybe (return ())
              (\m -> mboxSend mbox
                             (Short erl)
                             (Right "p")
                             (mboxSelf mbox, m)) 
              toErlang
        fromErlang <- timeout 1000 $ mboxRecv mbox
        maybe (return ())
              (\m -> atomically $ writeTChan writeChan m)
              fromErlang
        loop mbox

erlangMain :: String -- ^ On the form "moduleName:functionName"
           -> Spec ErlType a -- ^ The specification
           -> IO ()
erlangMain modfun spec = do
  let mod = takeWhile (/= ':') modfun
      fun = tail $ dropWhile (/= ':') modfun
  ph <- spawnCommand "erl -sname erl > /dev/null"
  threadDelay 2000000
  options <- newSession mod fun "erl"
  imp <- erlang options
  sessionCheck imp spec 
  killErlangNode "erl"

-- Kill an erlang node
killErlangNode :: String -> IO ()
killErlangNode name = do
  port <- readCreateProcess
    (shell $ "epmd -names | awk -v name=" ++ name ++ " '$2==name {print $5}'")
    ""
  pid <- readCreateProcess
    (shell $ "lsof -i TCP:" ++ port
           ++ " -s TCP:LISTEN | tail -n +2 | awk '{print $2}'")
    ""
  void $ createProcess (shell $ "kill " ++ pid) 
