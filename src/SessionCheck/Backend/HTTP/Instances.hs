{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances #-}
module SessionCheck.Backend.HTTP.Instances where

import Test.QuickCheck
import Text.Read

import SessionCheck.Backend.HTTP.Types
import SessionCheck.Classes

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

instance Monoid (HTTPDescriptor s) where
  mempty = Desc Nothing Nothing Nothing

  l `mappend` r = Desc (maybe (descStartLine l) Just (descStartLine r))
                       (maybe (descUrl l) Just (descUrl r))
                       (maybe (descParameters l) Just (descParameters r))

instance Arbitrary Method where
  arbitrary = elements [GET, POST]

instance Arbitrary Status where
  arbitrary = StatusCode . abs <$> arbitrary

instance IsHTTPBody a => HTTPRequest a :< HTTPData where
  inj msg = HTTP { httpMethod     = show (requestMethod msg)
                 , httpUrl        = requestUrl msg
                 , httpParameters = requestParameters msg
                 , httpBody       = body (requestBody msg) }

  prj http = do
    body   <- parseBody (httpBody http)
    method <- readMaybe (httpMethod http)
    return $ HTTPRequest { requestMethod     = method
                         , requestUrl        = httpUrl http
                         , requestParameters = httpParameters http
                         , requestBody       = body }
