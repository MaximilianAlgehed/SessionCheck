module SessionCheck.Backend.Erlang where

import Foreign.Erlang
import Control.Concurrent.STM
import Data.IORef

import SessionCheck.Backend

data Options =
  Options { targetModule   :: String
          , targetFunction :: String
          , self           :: Self }

-- Should only be called once
newSession :: String -- ^ Target module
           -> String -- ^ Target function
           -> IO Options
newSession mod fun = do
  self <- createSelf "haskell@localhost"
  return $ Options mod fun self

-- Generate an `Implementation` from the options
erlang :: Options -> IO (Implementation ErlType)
erlang opts = do
  readChan  <- atomically $ newTChan
  writeChan <- atomically $ newTChan
  isDead    <- newIORef False
  return $ Imp { outputChan = readChan
               , inputChan  = writeChan
               , dead       = isDead 
               , run        = makeRunFun readChan writeChan isDead opts }

makeRunFun :: TChan ErlType
           -> TChan ErlType 
           -> IORef Bool
           -> Options
           -> IO ()
makeRunFun readChan writeChan dead opts = do
  mbox <- createMBox (self opts)
  let pid = mboxSelf mbox
  rpcCall mbox (Short "erl") (targetModule opts) (targetFunction opts) []
  mboxSend mbox (Short "erl") (Right "p") (mboxSelf mbox)
  -- TODO: Do the communication between the input and output
  --       and take in to account the `dead` IORef
