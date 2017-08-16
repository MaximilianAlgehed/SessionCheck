module SessionCheck.Test where

import Control.Concurrent

import SessionCheck.Spec
import SessionCheck.Backend
import SessionCheck.Evaluate

sessionCheck :: Show t => Implementation t -> Spec t a -> IO ()
sessionCheck imp spec = do
  loop 100
  where
    loop 0 = print "OK"
    loop n = do
      forkIO $ run imp
      status <- evaluate imp spec
      if isError status then
        print status
      else
        loop (n-1)
