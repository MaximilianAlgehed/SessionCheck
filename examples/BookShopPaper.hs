{-# LANGUAGE TypeOperators
           , FlexibleContexts #-}
module BookShop where

import SessionCheck
import SessionCheck.Backend.Erlang

bookShop :: (Int :< t, String :< t, [Int] :< t) => Spec t ()
bookShop = bookShopLoop []

bookShopLoop :: (Int :< t, String :< t, [Int] :< t) => [Int] -> Spec t ()
bookShopLoop books = do
  choice <- choose [ "buy", "request", "exit" ]
  case choice of
    "buy" -> do
      book <- send anyInt
      bookShopLoop (book : books)
    "request" -> do
      get (permutationOf books)
      bookShopLoop books
    "exit" -> do
      stop

main :: IO ()
main = do
  erlangMain "bookShopPaper:main" bookShop
