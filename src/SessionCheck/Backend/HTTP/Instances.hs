{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances #-}
module SessionCheck.Backend.HTTP.Instances where

import Test.QuickCheck

import SessionCheck.Backend.HTTP.Types

class IsHTTPBody a where
  body      :: a -> String
  parseBody :: String -> Maybe a

instance IsHTTPBody EmptyBody where
  body _ = ""

  parseBody "" = Just EmptyBody
  parseBody _  = Nothing

instance Arbitrary EmptyBody where
  arbitrary = return EmptyBody

  shrink _ = []

instance Monoid HTTPDescriptor where
  mempty = Desc Nothing Nothing Nothing

  l `mappend` r = Desc (maybe (descMethod l) Just (descMethod r))
                       (maybe (descUrl l) Just (descUrl r))
                       (maybe (descParameters l) Just (descParameters r))

instance Arbitrary Method where
  arbitrary = elements [GET, POST]
