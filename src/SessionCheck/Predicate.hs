module SessionCheck.Predicate where

import Test.QuickCheck

import SessionCheck.Types 

anyInt :: Predicate Int
anyInt =  Predicate { apply     = const True
                    , satisfies = arbitrary
                    , name      = "anyInt" }
