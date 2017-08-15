{-# LANGUAGE GADTs #-}
module SessionCheck.Types where

import Control.Monad.Cont
import Test.QuickCheck

data Predicate a =
  Predicate { apply    :: a -> Bool
            , generate :: Gen a
            }

data Spec a where
  Get   :: Predicate a -> (a -> Spec b) -> Spec b
  Send  :: Predicate a -> (a -> Spec b) -> Spec b
  Both  :: Spec a -> Spec b -> (() -> Spec a) -> Spec a
