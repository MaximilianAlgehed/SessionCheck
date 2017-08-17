module SessionCheck.Test where

import Control.Concurrent

import SessionCheck.Spec
import SessionCheck.Backend
import SessionCheck.Evaluate

sessionCheck :: Show t => Implementation t -> Spec t a -> IO ()
sessionCheck imp spec = do
  loop 100
  where
    loop 0 = putStrLn "\nOK"
    loop n = do
      putStr "."
      reset imp
      forkIO $ run imp
      (status, l) <- evaluate imp spec
      if isError status then do
        putStrLn $ "\nFailed with:\n" ++ show status
        putStrLn $ "\nWith trace:\n" ++ printTrace l ++ "\n"
      else
        loop (n-1)
