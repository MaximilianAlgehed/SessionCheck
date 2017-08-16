module SessionCheck.Backend where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad
import System.Timeout
import Data.IORef

data Implementation t = Imp { outputChan :: TChan t
                            , inputChan  :: TChan t
                            , dead       :: IORef Bool
                            , run        :: IO () }

changeDirection :: Implementation t -> Implementation t
changeDirection imp = imp { outputChan = inputChan imp
                          , inputChan  = outputChan imp }

peek :: Implementation t -> IO (Maybe t)
peek imp = do
  d <- readIORef (dead imp)
  if d then
    return Nothing
  else
    timeout (10^6) (atomically . peekTChan . inputChan $ imp)

pop :: Implementation t -> IO ()
pop = void . atomically . readTChan . inputChan

kill :: Implementation t -> IO ()
kill imp = atomicWriteIORef (dead imp) True
