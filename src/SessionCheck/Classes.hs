{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
#-}
module SessionCheck.Classes where

class (a :< b) where
  inj :: a -> b
  prj :: b -> Maybe a
