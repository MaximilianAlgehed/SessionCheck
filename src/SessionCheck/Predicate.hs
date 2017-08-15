{-# LANGUAGE TypeOperators #-}
module SessionCheck.Predicate where

import Test.QuickCheck

import SessionCheck.Classes

data Predicate a =
  Predicate { apply     :: a -> Bool
            , satisfies :: Gen a
            , name      :: String
            }

anyInt :: Predicate Int
anyInt =  Predicate { apply     = const True
                    , satisfies = arbitrary
                    , name      = "anyInt" }

test :: a :< t => Predicate a -> t -> Bool
test p t = maybe False id (apply p <$> prj t)
