{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances #-}
module SessionCheck.Backend.HTTP.Instances where

import SessionCheck.Backend.HTTP.Types
import SessionCheck.Classes

class HTTPLike a where
  request  :: a -> Bool

instance ( HTTPLike a
         , a :< HTTPData ) => a :< RequestReply where
  inj a
    | request a = Request . inj $ a
    | otherwise = Reply   . inj $ a

  prj (Request d) = prj d
  prj (Reply d)   = prj d
