{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module SessionCheck.Backend.Coherence.Instances where

import Data.Dynamic
import Data.Typeable

import SessionCheck.Classes

instance Typeable a => a :< Dynamic where
  inj = toDyn 
  prj = fromDynamic
