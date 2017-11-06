{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
module SessionCheck.Backend.HTTP.Types where

import SessionCheck.Predicate

data HTTPData = HTTP { httpMethod     :: String
                     , httpUrl        :: String
                     , httpParameters :: [(String, String)]
                     , httpBody       :: String }
                     deriving (Ord, Eq, Show)

data Method = GET
            | POST
            deriving (Ord, Eq, Show, Read)

data HTTPMessage a = HTTPMessage { messageMethod     :: Method
                                 , messageUrl        :: String 
                                 , messageParameters :: [(String, String)]
                                 , messageBody       :: a }
                                 deriving (Ord, Eq, Show)

data HTTPDescriptor = Desc { descMethod     :: Maybe (Predicate Method)
                           , descUrl        :: Maybe (Predicate String)
                           , descParameters :: Maybe (Predicate [(String, String)]) }-- Unsure about this case

data EmptyBody = EmptyBody deriving (Ord, Eq, Show)
