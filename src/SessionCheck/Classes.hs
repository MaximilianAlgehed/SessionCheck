{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables
#-}
module SessionCheck.Classes where

-- A subtyping relation
class (a :< b) where
  inj :: a -> b
  prj :: b -> Maybe a

-- Trivial subtyping
instance a :< a where
  inj = id
  prj = Just 

-- Subtyping for maybes
instance a :< b => a :< Maybe b where
  inj = Just . inj
  prj = (>>= prj)
