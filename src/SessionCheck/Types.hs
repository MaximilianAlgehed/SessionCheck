{-# LANGUAGE GADTs, TypeOperators #-}
module SessionCheck.Types where

import Control.Monad

import SessionCheck.Classes
import SessionCheck.Predicate

test :: a :< t => Predicate a -> t -> Bool
test p t = maybe False id (apply p <$> prj t)

data Spec t a where
  Get   :: a :< t => Predicate a -> Spec t a
  Send  :: a :< t => Predicate a -> Spec t a
  Fork  :: Spec t a -> Spec t ()
  Stop  :: Spec t a
  -- Monadic fragment
  Return :: a -> Spec t a
  Bind   :: Spec t a -> (a -> Spec t b) -> Spec t b

instance Monad (Spec t) where
  return = Return
  (>>=)  = Bind

instance Applicative (Spec t) where
  pure  = Return
  (<*>) = ap

instance Functor (Spec t) where
  fmap = liftM

get :: a :< t => Predicate a -> Spec t a
get = Get

send :: a :< t => Predicate a -> Spec t a
send = Send

fork :: Spec t a -> Spec t ()
fork = Fork

(//) :: Spec t a -> Spec t b -> Spec t ()
l // r = fork l >> r >> return ()

stop :: Spec t a
stop = Stop

dual :: Spec t a -> Spec t a
dual s = case s of
  Get p    -> Send p
  Send p   -> Get p
  Fork s   -> Fork (dual s)
  Stop     -> Stop
  Return a -> Return a
  Bind s f -> Bind (dual s) (dual . f)
