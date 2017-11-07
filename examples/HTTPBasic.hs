{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module HTTPBasic where

import SessionCheck
import SessionCheck.Backend.HTTP

protocol :: HTTPMessage EmptyBody :< t => Spec t (HTTPMessage EmptyBody)
protocol = do
  send $ emptyBody `with` url (is "hello") <> method (is GET) <> parameters (are [])
