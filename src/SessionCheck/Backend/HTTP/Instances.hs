{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances #-}
module SessionCheck.Backend.HTTP.Instances where

import Test.QuickCheck
import Text.Read
import Data.Maybe

import SessionCheck.Backend.HTTP.Types
import SessionCheck.Classes

class IsHTTPBody a where
  body      :: a -> String
  parseBody :: String -> Maybe a

instance IsHTTPBody a => HTTPRequest a :< HTTPMessage where
  inj = Request . fmap body

  prj (Request r) = do
    body' <- parseBody (requestBody r)
    return (fmap (const body') r)
  prj _ = Nothing

instance IsHTTPBody a => HTTPReply a :< HTTPMessage where
  inj = Reply . fmap body

  prj (Reply r) = do
    body' <- parseBody (replyBody r)
    return (fmap (const body') r)
  prj _ = Nothing

instance IsHTTPBody EmptyBody where
  body _ = ""

  parseBody "" = Just EmptyBody
  parseBody _  = Nothing

instance IsHTTPBody String where
  body = id
  
  parseBody = Just 

instance Arbitrary EmptyBody where
  arbitrary = return EmptyBody

  shrink _ = []

instance Monoid (HTTPDescriptor s) where
  mempty = Desc Nothing Nothing Nothing

  l `mappend` r = Desc (maybe (descStartLine l) Just (descStartLine r))
                       (maybe (descUrl l) Just (descUrl r))
                       (maybe (descHeaders l) Just (descHeaders r))

instance Arbitrary Method where
  arbitrary = elements [GET, POST]

instance Arbitrary Status where
  arbitrary = StatusCode . abs <$> arbitrary
