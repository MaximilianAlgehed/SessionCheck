{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
module SessionCheck.Backend.HTTP.Types where

data HTTPData = HTTP { httpMethod     :: String
                     , httpUrl        :: String
                     , httpParameters :: [(String, String)]
                     , httpBody       :: String }
                     deriving (Ord, Eq, Show)

data HTTPMessage a = HTTPMessage { messageMethod     :: String
                                 , messageUrl        :: String
                                 , messageParameters :: [(String, String)]
                                 , messageBody       :: a }

data EmptyBody = EmptyBody deriving (Ord, Eq, Show)
