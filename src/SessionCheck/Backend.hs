module SessionCheck.Backend where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad
import System.Timeout
import Data.IORef

data Implementation t = Imp { outputChan :: TChan t
                            , inputChan  :: TChan t
                            , dead       :: IORef Bool
                            , done       :: MVar ()
                            , run        :: IO () }

peek :: Implementation t -> IO (Maybe t)
peek imp = do
  d <- readIORef (dead imp)
  if d then
    return Nothing
  else
    timeout (3*10^6) (atomically . peekTChan . inputChan $ imp)

pop :: Implementation t -> IO ()
pop = void . atomically . readTChan . inputChan

kill :: Implementation t -> IO ()
kill imp = do
  atomicWriteIORef (dead imp) True
  takeMVar (done imp)

clearTChan :: TChan t -> IO ()
clearTChan tc = do
  done <- atomically $ isEmptyTChan tc
  unless done $ do
     atomically $ readTChan tc
     clearTChan tc

reset :: Implementation t -> IO ()
reset imp = do
  writeIORef (dead imp) False
  tryTakeMVar (done imp)
  clearTChan (outputChan imp)
  clearTChan (inputChan imp)
