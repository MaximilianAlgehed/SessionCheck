{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SessionCheck.Backend.Erlang.Types where

import Control.DeepSeq
import Test.QuickCheck

newtype Atom = Atom String deriving (Ord, Eq, NFData)

instance Show Atom where
  show (Atom s) = s

instance Arbitrary Atom where
  arbitrary = do
    suf <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'])
    fst <- elements ['a'..'z']
    return (atom $ fst : suf)
  shrink (Atom s) = Atom <$> filter (not . null) (shrink s)

atom :: String -> Atom
atom = Atom
