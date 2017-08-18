{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.Coherence.Main ( checkCoherence )where

import SessionCheck.Classes
import SessionCheck.Spec 
import SessionCheck.Evaluate
import SessionCheck.Backend
import SessionCheck.Test

import Control.Concurrent
import Control.Monad
import Data.Dynamic
import Data.IORef

checkCoherence :: Spec Dynamic a -> IO ()
checkCoherence s = do
  imp <- clean 
  sessionCheck (imp { run = runFun imp s}) s
  where
    runFun :: Implementation Dynamic -> Spec Dynamic a -> IO ()
    runFun imp s = do
      tid <- forkIO $ void $ evaluate imp (dual s)
      loop imp
      killThread tid
      putMVar (done imp) ()

    loop imp = do
      d <- readIORef (dead imp)
      threadDelay 100
      unless d (loop imp)
