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

checkCoherence :: Spec Dynamic a -> IO ()
checkCoherence s = do
  imp <- clean 
  sessionCheck (imp { run = runFun imp s}) s
  where
    runFun :: Implementation Dynamic -> Spec Dynamic a -> IO ()
    runFun imp s = do
      tid <- forkIO $ void $ evaluate (swapDirection imp) (dual s)
      readMVar (dead imp)
      killThread tid
      putMVar (done imp) ()
