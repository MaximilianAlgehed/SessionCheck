{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.Coherence.Main ( checkCoherence )where

import SessionCheck.Classes
import SessionCheck.Spec 
import SessionCheck.Evaluate
import SessionCheck.Backend
import SessionCheck.Test
import SessionCheck.Backend.Coherence.Types

import Control.Concurrent
import Control.Monad
import Data.Dynamic

-- Check that a spec can be run against itself without error
-- Useful for finding deadlocks (arising from uses of interleave)
-- and unsatisfiable predicates
checkCoherence :: Spec DynamicValue a -> IO ()
checkCoherence s = do
  imp <- clean 
  sessionCheck (imp { run = runFun imp s}) s
  where
    runFun :: Implementation DynamicValue -> Spec DynamicValue a -> IO ()
    runFun imp s = do
      tid <- forkIO $ void $ evaluate (swapDirection imp) (dual s)
      readMVar (dead imp)
      killThread tid
      putMVar (done imp) ()
