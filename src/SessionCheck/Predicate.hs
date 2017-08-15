{-# LANGUAGE TypeOperators #-}
module SessionCheck.Predicate where

import Test.QuickCheck

import SessionCheck.Classes

-- A representation of dualizable predicates 
data Predicate a =
  Predicate { apply     :: a -> Bool
            , satisfies :: Gen a
            , name      :: String }

-- Test if a `t` satisfies a predicate for `a`s when `a :< t`
test :: a :< t => Predicate a -> t -> Bool
test p t = maybe False id (apply p <$> prj t)

-- The predicate which accepts anything
anything :: Arbitrary a => Predicate a
anything = Predicate { apply     = const True
                     , satisfies = arbitrary
                     , name      = "anything" }

-- Accepts any `Int`
anyInt :: Predicate Int
anyInt = anything { name = "anyInt" }

-- Accepts any `Double`
anyDouble :: Predicate Int
anyDouble = anything { name = "anyDouble" }

-- Accepts any `Dobule`
anyBool :: Predicate Int
anyBool = anything { name = "anyBool" }

