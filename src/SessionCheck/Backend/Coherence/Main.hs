{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module SessionCheck.Backend.Coherence.Main ( checkCoherence )where

import SessionCheck.Classes
import SessionCheck.Spec 

import Data.Dynamic

checkCoherence :: Spec Dynamic a -> IO ()
checkCoherence = undefined
