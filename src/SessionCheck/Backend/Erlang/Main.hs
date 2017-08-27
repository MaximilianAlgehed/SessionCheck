module SessionCheck.Backend.Erlang.Main ( erlangMain )where

import Foreign.Erlang
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
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
  imp <- clean
  return $ imp { run = runFun imp opts }

runFun :: Implementation ErlType
       -> Options
       -> IO ()
runFun imp@(Imp readChan writeChan _ _ done _) opts = do
  mbox <- createMBox (self opts)
  rpcCall mbox (Short erl) (targetModule opts) (targetFunction opts) []
  mboxSend mbox (Short erl) (Right "p") (mboxSelf mbox)
  tid <- forkIO $ erlLoop mbox
  loop mbox
  killThread tid
  pid <- rpcCall mbox (Short erl) "erlang" "whereis" [ErlAtom "p"]
  void $ rpcCall mbox (Short erl) "erlang" "exit" [pid, ErlAtom "ok"] 
  putMVar done ()
  where
    erl = erlangNode opts
    erlLoop mbox = do
      m <- mboxRecv mbox
      atomically $ writeTChan writeChan m
      erlLoop mbox

    loop mbox = do
      d <- isDead imp
      unless d $ do
        toErlang <- timeout 1000 $ atomically $ readTChan readChan
        maybe (return ())
              (\m -> mboxSend mbox
                             (Short erl)
                             (Right "p")
                             (mboxSelf mbox, m)) 
              toErlang
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
  p <- readCreateProcess
    (shell $ "epmd -names | awk -v name=" ++ name ++ " '$2==name {print $5}'")
    ""
  let port = filter (/='\n') p
  pid <- readCreateProcess
    (shell $ "lsof -i TCP:" ++ port
           ++ " -s TCP:LISTEN | tail -n +2 | awk '{print $2}'")
    ""
  void $ createProcess (shell $ "kill " ++ pid) 
