{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module HTTPBasic where

import SessionCheck
import SessionCheck.Backend.HTTP

protocol :: ( HTTPReply EmptyBody :< t
            , HTTPRequest EmptyBody :< t) => Spec t (HTTPReply EmptyBody)
protocol = do
  send $ emptyBody `with` url (is "hello") <> method (is GET) <> parameters (are [])
  get  $ emptyBody `with` status (is $ StatusCode 200)
