{-# LANGUAGE TypeOperators
           , FlexibleContexts #-}
module SMTP where

import Control.Monad
import Test.QuickCheck

import SessionCheck

type Domain = String
type ForwardPath = String -- Approximation
type ReversePath = ForwardPath 

data SMTPCommand = HELO Domain
                 | MAIL_FROM ReversePath
                 | RCPT_TO ForwardPath
                 | DATA
                 | RSET
                 | SEND_FROM ReversePath
                 | SOML_FROM ReversePath
                 | SAML_FROM ReversePath
                 | VRFY String
                 | EXPN String
                 | HELP (Maybe String)
                 | NOOP
                 | QUIT
                 | TURN
                 deriving (Ord, Eq, Show)
