{-# LANGUAGE TypeOperators
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , UndecidableInstances #-}
module SessionCheck.Backend.Erlang.Instances ( ErlType ) where

import Foreign.Erlang
import Data.Char
import Data.Maybe

import SessionCheck.Classes
import SessionCheck.Backend.Erlang.Types

instance Int :< ErlType where
    inj x             
       | abs x <= 0x7FFFFFFF = ErlInt x        
       | otherwise           = ErlBigInt (fromIntegral x) -- Haskell Int (might) use 64 bits whether erlang's small Int use only 32 bit

    prj (ErlInt x)    = Just x
    prj (ErlBigInt x) = Just $ fromIntegral x
    prj _ = Nothing

instance Double :< ErlType where
    inj   x            = ErlFloat x 
    prj (ErlFloat x) = Just x
    prj _ = Nothing

instance Float :< ErlType where
    inj x              = ErlFloat (realToFrac x)
    prj (ErlFloat x) = Just $ realToFrac x
    prj _ = Nothing

instance Integer :< ErlType where
    inj   x             = ErlBigInt x
    prj (ErlInt x)    = Just $ fromIntegral x
    prj (ErlBigInt x) = Just x
    prj _ = Nothing

instance String :< ErlType where
    inj   x           = ErlString x
    prj ErlNull       = Just $ ""
    prj (ErlString x) = Just $ x
    prj (ErlAtom x)   = Just $ x
    prj (ErlList xs)  = Just $ map (chr . fromErlang) xs
    prj _ = Nothing

instance Bool :< ErlType where
    inj   True              = ErlAtom "true"
    inj   False             = ErlAtom "false"
    prj (ErlAtom "true")  = Just True
    prj (ErlAtom "false") = Just False
    prj _ = Nothing

instance [ErlType] :< ErlType where
    inj   []           = ErlNull
    inj   xs           = ErlList xs
    prj ErlNull      = Just []
    prj (ErlList xs) = Just xs
    prj _ = Nothing

instance {-# OVERLAPPABLE #-} (a :< ErlType) => [a] :< ErlType where
    inj [] = ErlNull
    inj xs = ErlList . map inj $ xs

    prj ErlNull      = Just []
    prj (ErlList xs) = sequence $ map prj xs
    prj _            = Nothing

instance (a :< ErlType, b :< ErlType) => (a, b) :< ErlType where
    inj   (x, y)            = ErlTuple [inj x, inj y]
    prj (ErlTuple [x, y]) = do
                                    l <- prj x
                                    r <- prj y
                                    return (l, r)
    prj _ = Nothing

instance (a :< ErlType, b :< ErlType, c :< ErlType) => (a, b, c) :< ErlType where
    inj   (x, y, z)         = ErlTuple [inj x, inj y, inj z]
    prj (ErlTuple [x, y, z]) = do
                                    l <- prj x
                                    r <- prj y
                                    f <- prj z
                                    return (l, r, f)
    prj _ = Nothing

instance (a :< ErlType) => Maybe a :< ErlType where
  inj Nothing  = ErlAtom "nothing"
  inj (Just x) = ErlTuple [ErlAtom "just", inj x]

  prj (ErlAtom "nothing") = Nothing
  prj (ErlTuple [ErlAtom "just", x]) =
    do
      x <- prj x
      return (Just x)
  prj _ = Nothing

instance Atom :< ErlType where
  inj (Atom s)    = ErlAtom s
  prj (ErlAtom s) = Just (Atom s)
  prj _           = Nothing

instance (t :< ErlType) => Erlang t where
    toErlang = inj
    fromErlang = fromJust . prj -- unsafe
