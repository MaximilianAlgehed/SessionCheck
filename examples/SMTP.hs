{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , DeriveGeneric
           , DeriveAnyClass #-}
module SMTP where

import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Control.DeepSeq
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as P

import SessionCheck

-- Approximations
type Domain = String
type ForwardPath = String
type ReversePath = ForwardPath 

-- Minimal subset of SMTP
data SMTPCommand = HELO Domain
                 | MAIL_FROM ReversePath
                 | RCPT_TO ForwardPath
                 | DATA
                 | RSET
                 | NOOP
                 | QUIT
                 deriving (Ord, Eq, Show, Generic, NFData)

instance Arbitrary SMTPCommand where
  arbitrary = oneof [ HELO      <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , MAIL_FROM <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , RCPT_TO   <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , return DATA
                    , return RSET
                    , return NOOP
                    , return QUIT ]

smtpCommandParser :: ReadP SMTPCommand
smtpCommandParser = foldr (+++) pfail [ heloParser
                                      , mailFromParser
                                      , rcptToParser
                                      , dataParser
                                      , rsetParser
                                      , noopParser
                                      , quitParser ]
  where
    heloParser = do
      string "HELO"
      skipSpaces
      HELO <$> manyTill P.get eof
    mailFromParser = do
      string "MAIL FROM:"
      skipSpaces
      MAIL_FROM <$> manyTill P.get eof
    rcptToParser = do
      string "RCPT TO:"
      skipSpaces
      RCPT_TO <$> manyTill P.get eof
    dataParser = do
      string "DATA"
      eof
      return DATA
    rsetParser = do
      string "RSET"
      eof
      return RSET
    noopParser = do
      string "NOOP"
      eof
      return NOOP
    quitParser = do
      string "QUIT"
      eof
      return QUIT

smtpCommandPrinter :: SMTPCommand -> String
smtpCommandPrinter c = case c of
  HELO d       -> "HELO " ++ d
  MAIL_FROM rp -> "MAIL FROM: " ++ rp
  RCPT_TO fp   -> "RCPT TO: " ++ fp 
  _            -> show c

prop_print_parse_command :: SMTPCommand -> Bool
prop_print_parse_command c = (readP_to_S smtpCommandParser) (smtpCommandPrinter c) == [(c, "")]

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

instance Arbitrary SMTPReply where
  arbitrary = oneof [ return R500 
                    , return R501
                    , return R502
                    , return R503
                    , return R504
                    , return R211
                    , return R214
                    , R220 <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , R221 <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , R421 <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , return R250 
                    , R251 <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")
                    , return R450
                    , return R550
                    , return R451
                    , R551 <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.") 
                    , return R452
                    , return R552
                    , return R553
                    , return R354
                    , return R554 ]

smtpReplyParser :: ReadP SMTPReply
smtpReplyParser = foldr (+++) pfail $ [ arged R220 "220"
                                      , arged R221 "221"
                                      , arged R421 "421"
                                      , arged R251 "251"
                                      , arged R551 "551" ]
                                      ++ map (\c -> (string . drop 1 . show $ c) >> return c)
                                           [ R500 
                                           , R501
                                           , R502
                                           , R503
                                           , R504
                                           , R211
                                           , R214
                                           , R250 
                                           , R450
                                           , R550
                                           , R451
                                           , R452
                                           , R552
                                           , R553
                                           , R354
                                           , R554 ]
  where
    arged con name = do
      string name
      skipSpaces
      con <$> manyTill P.get eof

smtpReplyPrinter :: SMTPReply -> String
smtpReplyPrinter c = case c of
  R220 s -> "220 " ++ s
  R221 s -> "221 " ++ s
  R421 s -> "421 " ++ s
  R251 s -> "251 " ++ s
  R551 s -> "551 " ++ s
  _      -> drop 1 $ show c

prop_print_parse_reply :: SMTPReply -> Bool
prop_print_parse_reply c = (readP_to_S smtpReplyParser) (smtpReplyPrinter c) == [(c, "")]

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
