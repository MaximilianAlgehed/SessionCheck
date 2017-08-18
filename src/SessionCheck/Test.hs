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
      case status of
        Left e -> do
          putStrLn $ "\nFailed with:\n" ++ show e 
          putStrLn $ "\nWith trace:\n" ++ printTrace l ++ "\n"
        _      -> loop (n-1)
