{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , DeriveGeneric
           , DeriveAnyClass
           , MultiParamTypeClasses #-}
module SMTP where

import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Control.DeepSeq
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as P

import SessionCheck
import SessionCheck.Backend.TCP

-- Approximations
type Domain = String
type ForwardPath = String
type ReversePath = ForwardPath 

niceString :: Gen String
niceString = listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ":/.")

-- Minimal subset of SMTP
data SMTPCommand = HELO Domain
                 | EHLO Domain
                 | MAIL_FROM ReversePath
                 | RCPT_TO ForwardPath
                 | DATA
                 | RSET
                 | NOOP
                 | QUIT
                 deriving (Ord, Eq, Show, Generic, NFData)

instance Arbitrary SMTPCommand where
  arbitrary = oneof [ HELO      <$> niceString
                    , HELO      <$> niceString
                    , MAIL_FROM <$> niceString 
                    , RCPT_TO   <$> niceString 
                    , return DATA
                    , return RSET
                    , return NOOP
                    , return QUIT ]

smtpCommandParser :: ReadP SMTPCommand
smtpCommandParser = foldr (+++) pfail [ heloParser
                                      , ehloParser
                                      , mailFromParser
                                      , rcptToParser
                                      , dataParser
                                      , rsetParser
                                      , noopParser
                                      , quitParser ]
  where
    heloParser = do
      string "HELO" +++ string "helo"
      skipSpaces
      HELO <$> manyTill P.get eof
    ehloParser = do
      string "EHLO" +++ string "ehlo"
      skipSpaces
      EHLO <$> manyTill P.get eof
    mailFromParser = do
      string "MAIL FROM:" +++ string "mail FROM:"
      skipSpaces
      MAIL_FROM <$> manyTill P.get eof
    rcptToParser = do
      string "RCPT TO:" +++ string "rcpt TO:"
      skipSpaces
      RCPT_TO <$> manyTill P.get eof
    dataParser = do
      string "DATA" +++ string "data"
      eof
      return DATA
    rsetParser = do
      string "RSET" +++ string "rset"
      eof
      return RSET
    noopParser = do
      string "NOOP" +++ string "noop"
      eof
      return NOOP
    quitParser = do
      string "QUIT" +++ string "quit"
      eof
      return QUIT

smtpCommandPrinter :: SMTPCommand -> String
smtpCommandPrinter c = case c of
  HELO d       -> "HELO " ++ d
  EHLO d       -> "EHLO " ++ d
  MAIL_FROM rp -> "MAIL FROM: " ++ rp
  RCPT_TO fp   -> "RCPT TO: " ++ fp 
  _            -> show c

prop_print_parse_command :: SMTPCommand -> Bool
prop_print_parse_command c = (readP_to_S smtpCommandParser) (smtpCommandPrinter c) == [(c, "")]

instance SMTPCommand :< TCPMessage where
  inj = TCPMessage . smtpCommandPrinter
  prj m = case readP_to_S smtpCommandParser (unTCPMessage m) of
    [(c, "")] -> Just c
    _         -> Nothing

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
prop_print_parse_reply c = readP_to_S smtpReplyParser (smtpReplyPrinter c) == [(c, "")]

instance SMTPReply :< TCPMessage where
  inj = TCPMessage . smtpReplyPrinter
  prj m = case readP_to_S smtpReplyParser (unTCPMessage m) of
    [(r, "")] -> Just r
    _         -> Nothing

heloMessage :: Predicate SMTPCommand 
heloMessage = Predicate { apply = \c -> case c of
                                          HELO _ -> True
                                          _      -> False
                        , satisfies = HELO <$> niceString
                        , shrunk    = \(HELO d) -> elements (HELO <$> shrink d)
                        , name      = "heloMessage" }

ehloMessage :: Predicate SMTPCommand 
ehloMessage = Predicate { apply = \c -> case c of
                                          EHLO _ -> True
                                          _      -> False
                        , satisfies = EHLO <$> niceString 
                        , shrunk    = \(EHLO d) -> elements (EHLO <$> shrink d)
                        , name      = "ehloMessage" }

mailMessage :: Predicate SMTPCommand
mailMessage = Predicate { apply = \c -> case c of
                                          MAIL_FROM _ -> True
                                          _           -> False
                        , satisfies = MAIL_FROM <$> niceString 
                        , shrunk    = \(MAIL_FROM d) -> elements (MAIL_FROM <$> shrink d)
                        , name      = "mailMessage" }

rcptMessage :: Predicate SMTPCommand
rcptMessage = Predicate { apply = \c -> case c of
                                          RCPT_TO _ -> True
                                          _         -> False
                        , satisfies = RCPT_TO <$> niceString 
                        , shrunk    = \(RCPT_TO d) -> elements (RCPT_TO <$> shrink d)
                        , name      = "rcptMessage" }

dataMessage :: Predicate SMTPCommand
dataMessage = (is DATA) { name = "dataMessage" }

endOfMail :: Predicate String
endOfMail = Predicate { apply     = (==".")
                      , satisfies = return "."
                      , shrunk    = \_ -> return []
                      , name      = "endOfMail" }

mail :: (String :< t, SMTPReply :< t, SMTPCommand :< t) => Spec t ()
mail = do
  msg <- get $ anyOf [ rcptMessage, dataMessage, is RSET]
  case msg of
    RSET -> stop
    RCPT_TO _ -> do
      send $ anyOf [is R250, is R550]
      mail
    DATA      -> do
      send $ is R354
      dataRecv

dataRecv :: (String :< t, SMTPReply :< t) => Spec t ()
dataRecv = do
  line <- get $ anyOf [ anything, is "." ]
  case line of
    "." -> void . send $ is R250
    _   -> dataRecv

handshakeRFC821 :: (SMTPCommand :< t, SMTPReply :< t) => Spec t ()
handshakeRFC821 = void $ do
  -- Handshake
  send heloMessage
  get  (is R250) -- Approximation
  get  heloMessage
  send (is R250) -- Approximation

handshakeRFC5321 :: (SMTPCommand :< t, SMTPReply :< t) => Spec t ()
handshakeRFC5321 = void $ do
  send heloMessage
  get  ehloMessage
  send (is R250)

smtp :: (String :< t, SMTPReply :< t, SMTPCommand :< t) => Spec t ()
smtp = do
  -- Perform the handshake
  --handshakeRFC821
  handshakeRFC5321
  loop
  where
    loop = do
      -- Choice of operations
      op <- get $ anyOf [ mailMessage, is QUIT, is RSET]
      case op of
        RSET -> stop
        MAIL_FROM _ -> do
          send (is R250) -- Approximation
          mail
          loop
        QUIT        -> stop

main :: IO ()
main = tcpMain Server "python SMTP.py 2> /dev/null < mail.txt" 252525 smtp
