{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
#-}
module SessionCheck.Classes where

class (a :< b) where
  inj :: a -> b
  prj :: b -> Maybe a

instance a :< a where
  inj = id
  prj = Just 

instance a :< Maybe a where
  inj = Just
  prj = id
