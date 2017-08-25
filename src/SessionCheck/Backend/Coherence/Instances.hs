{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module SessionCheck.Backend.Coherence.Instances where

import Data.Dynamic
import Data.Typeable

import SessionCheck.Backend.Coherence.Types
import SessionCheck.Classes

instance (Typeable a, Show a) => a :< DynamicValue where
  inj a = DV (toDyn a) (show a)
  prj = fromDynamic . dynamic
