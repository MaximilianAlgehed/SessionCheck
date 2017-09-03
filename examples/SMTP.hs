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

data SMTPReply = R500 
               | R501
               | R502
               | R503
               | R504
               | R211
               | R214
               | R220 Domain
               | R221 Domain
               | R421 Domain
               | R250 
               | R251 ForwardPath
               | R450
               | R550
               | R451
               | R551 ForwardPath
               | R452
               | R552
               | R553
               | R354
               | R554
               deriving (Ord, Eq, Show)

heloMessage :: Predicate STMPCommand
heloMessage = Predicate { apply = \c -> case c of
                                          HELO _ -> True
                                          _      -> False
                        , satisfies = HELO <$> arbitrary
                        , shrink    = \HELO d -> HELO <$> shrink d
                        , name      = "heloMessage" }
