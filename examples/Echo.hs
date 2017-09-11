{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Echo where

import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Control.DeepSeq
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as P

import SessionCheck
import SessionCheck.Backend.TCP

echo :: (String :< t) => Spec t ()
echo = do
  s <- send alphaNumString
  get (is s)
  return ()

main :: IO ()
main = tcpMain "Echo.py" 10000 echo
