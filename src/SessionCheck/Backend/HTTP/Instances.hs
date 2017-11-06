{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances #-}
module SessionCheck.Backend.HTTP.Instances where

import SessionCheck.Backend.HTTP.Types

class IsHTTPBody a where
  body      :: a -> String
  parseBody :: String -> Maybe a

instance IsHTTPBody EmptyBody where
  body _ = ""

  parseBody "" = Just EmptyBody
  parseBody _  = Nothing
