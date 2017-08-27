module SessionCheck.Types where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.MVar

-- The status of taking a single step in the execution of a protocol
data Status t = Sent t
              | Got  t
              | Done
              | Skip
              | GotBad String t
              | Step
              | Bad String
              | Amb String
              | Timeout String
              | Ok

instance Show t => Show (Status t) where
  show (Sent t)     = "Sent: " ++ show t
  show (Got t)      = "Got: " ++ show t
  show (GotBad p t) = "GotBad: " ++ p ++ " " ++ show t
  show Done         = "Done"
  show Skip         = "Skip"
  show Step         = "Step"
  show (Bad e)      = "Bad: " ++ e
  show (Amb e)      = "Ambiguity: " ++ e
  show (Timeout e)  = "Timeout: " ++ e
  show Ok           = "Ok"
