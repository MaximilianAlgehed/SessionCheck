{-# LANGUAGE TypeFamilies #-}
module SessionCheck.Backend.HTTP.Predicates where

import Test.QuickCheck

import SessionCheck.Predicate
import SessionCheck.Backend.HTTP.Types
import SessionCheck.Backend.HTTP.Instances

emptyBody :: Predicate EmptyBody
emptyBody = anything { name = "emptyBody" }

class With s where
  type RequestOrReply s :: * -> *

  with :: Predicate a -> HTTPDescriptor s -> Predicate (RequestOrReply s a)

instance With Method where
  type RequestOrReply Method = HTTPRequest

  with p_body desc = Predicate app
                               generate
                               (\_ -> fail "Not implemented")
                               "Name not yet implemented"
    where
      app msg =  apply p_body (requestBody msg)
              && testThe msg requestMethod descStartLine
              && testThe msg requestUrl descUrl
              && testThe msg requestHeaders descHeaders
  
      generate =  HTTPRequest
              <$> generator descStartLine
              <*> generator descUrl
              <*> generator descHeaders
              <*> satisfies p_body
  
      generator :: Arbitrary a => (HTTPDescriptor Method -> Maybe (Predicate a)) -> Gen a
      generator da = maybe arbitrary satisfies (da desc)
      
      testThe :: HTTPRequest a -> (HTTPRequest a -> b) -> (HTTPDescriptor Method -> Maybe (Predicate b)) -> Bool
      testThe msg ma da = maybe True (flip apply (ma msg)) (da desc)

instance With Status where
  type RequestOrReply Status = HTTPReply 

  with p_body desc = Predicate app
                               generate
                               (\_ -> fail "Not implemented")
                               "Name not yet implemented"
    where
      app msg =  apply p_body (replyBody msg)
              && testThe msg replyStatus descStartLine
              && testThe msg replyHeaders descHeaders
  
      generate =  HTTPReply
              <$> generator descStartLine
              <*> generator descHeaders
              <*> satisfies p_body
  
      generator :: Arbitrary a => (HTTPDescriptor Status -> Maybe (Predicate a)) -> Gen a
      generator da = maybe arbitrary satisfies (da desc)
      
      testThe :: HTTPReply a -> (HTTPReply a -> b) -> (HTTPDescriptor Status -> Maybe (Predicate b)) -> Bool
      testThe msg ma da = maybe True (flip apply (ma msg)) (da desc)

infixr 8 `with`

method :: Predicate Method -> HTTPDescriptor Method
method p = mempty { descStartLine = Just p }

status :: Predicate Status -> HTTPDescriptor Status
status p = mempty { descStartLine = Just p }

url :: Predicate String -> HTTPDescriptor s
url p = mempty { descUrl = Just p }

headers :: Predicate [(String, String)] -> HTTPDescriptor s
headers p = mempty { descHeaders = Just p }

(<>) :: HTTPDescriptor s -> HTTPDescriptor s -> HTTPDescriptor s
(<>) = mappend

infixr 9 <>
