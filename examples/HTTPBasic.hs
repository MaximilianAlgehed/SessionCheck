{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module HTTPBasic where

import SessionCheck
import SessionCheck.Backend.HTTP

protocol :: ( HTTPReply String :< t
            , HTTPRequest EmptyBody :< t) => Spec t (HTTPReply String)
protocol = do
  send $ emptyBody `with` url (is "/hello") <> method (is GET)
  get  $ (is "hello") `with` status (is $ StatusCode 200)
