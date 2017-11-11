{-# LANGUAGE MultiParamTypeClasses, GADTs, DeriveAnyClass, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module SessionCheck.Backend.HTTP.Types where

import Control.DeepSeq
import GHC.Generics

import SessionCheck.Predicate

data HTTPData = HTTP { httpMethod     :: String
                     , httpUrl        :: String
                     , httpParameters :: [(String, String)]
                     , httpBody       :: String }
                     deriving (Ord, Eq, Show, NFData, Generic)

data Method = GET
            | POST
            deriving (Ord, Eq, Show, Read, NFData, Generic)

newtype Status = StatusCode Int deriving (Ord, Eq, Show, NFData, Generic)

data HTTPRequest a = HTTPRequest { requestMethod     :: Method
                                 , requestUrl        :: String 
                                 , requestParameters :: [(String, String)]
                                 , requestBody       :: a }
                                 deriving (Ord, Eq, Show, NFData, Generic)


data HTTPReply a = HTTPReply { replyStatus     :: Status
                             , replyParameters :: [(String, String)]
                             , replyBody       :: a }
                             deriving (Ord, Eq, Show, NFData, Generic)



data HTTPDescriptor s = Desc { descStartLine  :: Maybe (Predicate s)
                             , descUrl        :: Maybe (Predicate String)
                             , descParameters :: Maybe (Predicate [(String, String)]) }

data EmptyBody = EmptyBody deriving (Ord, Eq, Show, NFData, Generic)
