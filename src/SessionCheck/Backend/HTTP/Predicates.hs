module SessionCheck.Backend.HTTP.Predicates where

import SessionCheck.Predicate
import SessionCheck.Backend.HTTP.Types
import SessionCheck.Backend.HTTP.Instances

emptyBody :: Predicate EmptyBody
emptyBody = anything
