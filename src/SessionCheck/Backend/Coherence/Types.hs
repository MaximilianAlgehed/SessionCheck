{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module SessionCheck.Backend.Coherence.Types where

import Data.Dynamic
import Data.Typeable

data DynamicValue = DV { dynamic :: Dynamic, string :: String }

instance Show DynamicValue where
  show = string
