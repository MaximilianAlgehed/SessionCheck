module SessionCheck.Backend.HTTP.Predicates where

import Test.QuickCheck

import SessionCheck.Predicate
import SessionCheck.Backend.HTTP.Types
import SessionCheck.Backend.HTTP.Instances

emptyBody :: Predicate EmptyBody
emptyBody = anything { name = "emptyBody" }

with :: Predicate a -> HTTPDescriptor -> Predicate (HTTPMessage a)
with p_body desc = Predicate app
                             generate
                             (\_ -> fail "Not implemented")
                             "Name not yet implemented"
  where
    app msg =  apply p_body (messageBody msg)
            && testThe msg messageMethod descMethod
            && testThe msg messageUrl descUrl
            && testThe msg messageParameters descParameters

    generate =  HTTPMessage
            <$> generator descMethod
            <*> generator descUrl
            <*> generator descParameters
            <*> satisfies p_body

    generator da = maybe arbitrary satisfies (da desc)
    
    testThe msg ma da = maybe True (flip apply (ma msg)) (da desc)

infixr 8 `with`

method :: Predicate Method -> HTTPDescriptor
method p = mempty { descMethod = Just p }

url :: Predicate String -> HTTPDescriptor
url p = mempty { descUrl = Just p }

parameters :: Predicate [(String, String)] -> HTTPDescriptor
parameters p = mempty { descParameters = Just p }

(<>) :: HTTPDescriptor -> HTTPDescriptor -> HTTPDescriptor
(<>) = mappend

infixr 9 <>
