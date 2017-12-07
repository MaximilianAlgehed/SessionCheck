{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveGeneric, MultiParamTypeClasses #-}

module BookShop where

import Test.QuickCheck
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Foreign.Erlang

import SessionCheck
import SessionCheck.Backend.Erlang

data Request = Order Int 
             | Checkout
             deriving (Generic)

data Reply = Basket { basket :: [Int] }
           deriving (Generic)

loop :: Request :< t => [Int] -> Spec t [Int]
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
  erlangMain "bookShop:main" clientSpec

instance Arbitrary Request where
  arbitrary = oneof [Order . (+1) . abs <$> arbitrary, return Checkout]

instance NFData Reply

instance Arbitrary Reply where
  arbitrary = Basket <$> arbitrary

instance NFData Request

supersetOfReply :: [Int] -> Predicate Reply
supersetOfReply = bimap Basket basket . supersetOf

anyRequest :: Predicate Request
anyRequest = anything

instance Request :< ErlType where
  inj Checkout  = ErlAtom "checkout"
  inj (Order s) = ErlTuple [ErlAtom "order", inj s]

  prj (ErlAtom "checkout") = Just Checkout
  prj (ErlTuple [ErlAtom "order", s]) = Order <$> prj s
  prj _ = Nothing

instance Reply :< ErlType where
  inj (Basket bs) = ErlList $ map inj bs

  prj = fmap Basket . prj 
