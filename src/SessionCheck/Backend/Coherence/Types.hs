{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module SessionCheck.Backend.Coherence.Types where

import Data.Dynamic
import Data.Typeable

-- Dynamic values and how they are printed
data DynamicValue = DV { dynamic :: Dynamic, string :: String }

-- There is no point in showing anything other than the string
-- representation of the DV
instance Show DynamicValue where
  show = string
