{-# LANGUAGE TypeOperators #-}
module SessionCheck.Predicate where

import Test.QuickCheck
import Data.List

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

-- Accepts any member of `as`
choiceOf :: (Eq a, Show a) => [a] -> Predicate a
choiceOf as = Predicate { apply     = flip elem as
                        , satisfies = elements as 
                        , name      = "choiceOf " ++ show as }

permutationOf :: (Eq a, Show a) => [a] -> Predicate [a]
permutationOf as = Predicate { apply     = \as' -> elem as' (permutations as)
                             , satisfies = shuffle as
                             , name      = "permutationOf " ++ show as }
