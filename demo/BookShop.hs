{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , DeriveGeneric #-}


module BookShop where

import Test.QuickCheck
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

import SessionCheck
import SessionCheck.Backend.Erlang

data Request = Order String
             | Checkout
             deriving (Generic)

data Reply   = Basket { basket :: [String] }
             deriving (Generic)

loop :: Request :< t => [String] -> Spec t [String]
loop books = do
  r <- send anyRequest
  case r of
    Order book -> loop (book : books)
    Checkout   -> return books

clientSpec :: (Request :< t, Reply :< t) => Spec t Reply
clientSpec = do
  ordered <- loop []
  get (supersetOfReply ordered)

main :: IO ()
main = do
  print ()
  --erlangMain "bookShop:main" clientSpec
  --

instance Arbitrary Request where
  arbitrary = oneof [Order <$> arbitrary, return Checkout]

instance NFData Reply

instance Arbitrary Reply where
  arbitrary = Basket <$> arbitrary

instance NFData Request

supersetOfReply :: [String] -> Predicate Reply
supersetOfReply = bimap Basket basket . supersetOf

anyRequest :: Predicate Request
anyRequest = anything
