{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.Coherence.Main ( checkCoherence, checkCompatibility )where

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
      readMVar (deadReason imp)
      killThread tid
      putMVar (done imp) ()

-- Check if two different protocols are compatible 
--
-- if `checkCompatibility a b` doesn't return an error, then
-- `a` and `b` are effectively two different versions of the
-- same protocol
checkCompatibility :: Spec DynamicValue a
                   -> Spec DynamicValue b
                   -> IO ()
checkCompatibility s s' = do
  imp <- clean
  putStrLn "Checking first protocol as primary"
  sessionCheck (imp { run = runFun imp s'}) s
  putStrLn "Checking second protocol as primary"
  sessionCheck (imp { run = runFun imp s}) s'
  where
    runFun :: Implementation DynamicValue -> Spec DynamicValue a -> IO ()
    runFun imp s = do
      tid <- forkIO $ void $ evaluate (swapDirection imp) (dual s)
      readMVar (dead imp)
      readMVar (deadReason imp)
      killThread tid
      putMVar (done imp) ()
