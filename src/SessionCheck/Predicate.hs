module SessionCheck.Predicate where

import Test.QuickCheck

data Predicate a =
  Predicate { apply     :: a -> Bool
            , satisfies :: Gen a
            , name      :: String
            }

anyInt :: Predicate Int
anyInt =  Predicate { apply     = const True
                    , satisfies = arbitrary
                    , name      = "anyInt" }
