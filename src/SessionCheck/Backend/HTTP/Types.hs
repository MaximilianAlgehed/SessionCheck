{-# LANGUAGE MultiParamTypeClasses, GADTs, DeriveAnyClass, DeriveGeneric #-}
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

data HTTPMessage a = HTTPMessage { messageMethod     :: Method
                                 , messageUrl        :: String 
                                 , messageParameters :: [(String, String)]
                                 , messageBody       :: a }
                                 deriving (Ord, Eq, Show, NFData, Generic)

data HTTPDescriptor = Desc { descMethod     :: Maybe (Predicate Method)
                           , descUrl        :: Maybe (Predicate String)
                           , descParameters :: Maybe (Predicate [(String, String)]) }

data EmptyBody = EmptyBody deriving (Ord, Eq, Show, NFData, Generic)
