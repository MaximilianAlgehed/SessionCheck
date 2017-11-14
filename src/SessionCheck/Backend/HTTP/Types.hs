{-# LANGUAGE MultiParamTypeClasses, GADTs, DeriveAnyClass, DeriveGeneric #-}
module SessionCheck.Backend.HTTP.Types where

import Control.DeepSeq
import GHC.Generics

import SessionCheck.Predicate

data ProtocolRole = Client
                  | Server
                  deriving (Ord, Eq, Show)

data HTTPMessage where
  Request :: HTTPRequest String -> HTTPMessage
  Reply   :: HTTPReply String -> HTTPMessage 
  deriving Show

data Method = GET
            | POST
            deriving (Ord, Eq, Show, Read, NFData, Generic)

newtype Status = StatusCode Int deriving (Ord, Eq, Show, NFData, Generic)

data HTTPRequest a = HTTPRequest { requestMethod  :: Method
                                 , requestUrl     :: String 
                                 , requestHeaders :: [(String, String)]
                                 , requestBody    :: a }
                                 deriving (Ord, Eq, Show, NFData, Generic)

instance Functor HTTPRequest where
  fmap f (HTTPRequest m u h a) = HTTPRequest m u h (f a)

data HTTPReply a = HTTPReply { replyStatus  :: Status
                             , replyHeaders :: [(String, String)]
                             , replyBody    :: a }
                             deriving (Ord, Eq, Show, NFData, Generic)

instance Functor HTTPReply where
  fmap f (HTTPReply s h a) = HTTPReply s h (f a)

data HTTPDescriptor s = Desc { descStartLine :: Maybe (Predicate s)
                             , descUrl       :: Maybe (Predicate String)
                             , descHeaders   :: Maybe (Predicate [(String, String)]) }

data EmptyBody = EmptyBody deriving (Ord, Eq, Show, NFData, Generic)
