{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , DeriveGeneric
           , DeriveAnyClass #-}
module SMTP where

import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Control.DeepSeq

import SessionCheck

-- Approximations
type Domain = String
type ForwardPath = String
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
                 deriving (Ord, Eq, Show, Generic, NFData)

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
               deriving (Ord, Eq, Show, Generic, NFData)

{- Consider implementing a function for doing these things,
 - the pattern matching bit in the case could be done using
 - exceptions to catch that the uninteresting bits where evaluated
 - and therefore the heads match (?), to this we can add some way of
 - doing the extraction in the implementation of `shrunk` (using "lenses"?)-}

heloMessage :: Predicate SMTPCommand 
heloMessage = Predicate { apply = \c -> case c of
                                          HELO _ -> True
                                          _      -> False
                        , satisfies = HELO <$> arbitrary
                        , shrunk    = \(HELO d) -> elements (HELO <$> shrink d)
                        , name      = "heloMessage" }

mailMessage :: Predicate SMTPCommand
mailMessage = Predicate { apply = \c -> case c of
                                          MAIL_FROM _ -> True
                                          _           -> False
                        , satisfies = MAIL_FROM <$> arbitrary
                        , shrunk    = \(MAIL_FROM d) -> elements (MAIL_FROM <$> shrink d)
                        , name      = "mailMessage" }

rcptMessage :: Predicate SMTPCommand
rcptMessage = Predicate { apply = \c -> case c of
                                          RCPT_TO _ -> True
                                          _         -> False
                        , satisfies = RCPT_TO <$> arbitrary
                        , shrunk    = \(RCPT_TO d) -> elements (RCPT_TO <$> shrink d)
                        , name      = "rcptMessage" }

dataMessage :: Predicate SMTPCommand
dataMessage = (is DATA) { name = "dataMessage" }

endOfMail :: Predicate String
endOfMail = Predicate { apply     = (==".")
                      , satisfies = return "."
                      , shrunk    = \_ -> return []
                      , name      = "endOfMail" }

mail :: (String :< t, SMTPReply :< t, SMTPCommand :< t) => Spec t SMTPReply
mail = do
  msg <- get $ anyOf [ rcptMessage, dataMessage ]
  case msg of
    RCPT_TO _ -> do
      send $ anyOf [is R250, is R550]
      mail
    DATA      -> do
      send $ is R354
      dataRecv

dataRecv :: (String :< t, SMTPReply :< t) => Spec t SMTPReply
dataRecv = do
  line <- get $ anyOf [ anything, is "." ]
  case line of
    "." -> send $ is R250
    _   -> dataRecv

smtp :: (String :< t, SMTPReply :< t, SMTPCommand :< t) => Spec t ()
smtp = do
  -- Handshake
  get  heloMessage
  send (is R250) -- Approximation
  send heloMessage
  get  (is R250) -- Approximation

  -- Choice of operations
  op <- get $ anyOf [ mailMessage, is QUIT ]
  case op of
    MAIL_FROM _ -> do
      send (is R250) -- Approximation
      mail
      smtp
    QUIT        -> stop
