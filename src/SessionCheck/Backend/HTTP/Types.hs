{-# LANGUAGE MultiParamTypeClasses #-}
module SessionCheck.Backend.HTTP.Types where

data HTTPData = HTTP { httpMethod     :: String
                     , httpUrl        :: String
                     , httpParameters :: [(String, String)]
                     , httpBody       :: String }
                     deriving (Ord, Eq, Show)

data EmptyBody = EmptyBody deriving (Ord, Eq, Show)
