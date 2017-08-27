module SessionCheck.Backend where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad
import System.Timeout

data Implementation t = Imp { outputChan :: TChan t
                            , inputChan  :: TChan t
                            , dead       :: MVar ()
                            , done       :: MVar ()
                            , run        :: IO () }

swapDirection :: Implementation t -> Implementation t
swapDirection imp = imp { outputChan = inputChan imp
                        , inputChan  = outputChan imp }

clean :: IO (Implementation t)
clean = do
  oc <- atomically $ newTChan 
  ic <- atomically $ newTChan
  d  <- newEmptyMVar
  mv <- newEmptyMVar
  return $ Imp oc ic d mv (return ())

peek :: Implementation t -> IO (Maybe t)
peek imp = timeout (10^3) (atomically . peekTChan . inputChan $ imp)

peekLong :: Implementation t -> IO (Maybe t)
peekLong imp = timeout (10^6) (atomically . peekTChan . inputChan $ imp)

pop :: Implementation t -> IO ()
pop = void . atomically . readTChan . inputChan

isDead :: Implementation t -> IO Bool
isDead imp = not <$> isEmptyMVar (dead imp)

kill :: Implementation t -> IO ()
kill imp = do
  tryPutMVar (dead imp) ()
  takeMVar   (done imp)

clearTChan :: TChan t -> IO ()
clearTChan tc = do
  done <- atomically $ isEmptyTChan tc
  unless done $ do
     atomically $ readTChan tc
     clearTChan tc

reset :: Implementation t -> IO ()
reset imp = do
  tryTakeMVar (dead imp)
  tryTakeMVar (done imp)
  clearTChan (outputChan imp)
  clearTChan (inputChan imp)
