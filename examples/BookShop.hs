{-# LANGUAGE TypeOperators
           , FlexibleContexts #-}
module BookShop where

import SessionCheck
import SessionCheck.Backend.Erlang

bookShop :: (Int :< t, String :< t, [Int] :< t) => [Int] -> Spec t ()
bookShop books = do 
  b <- send posInt --anyInt
  let books' = b : books

  choice <- choose ["another", "request"]
  case choice of
    "another" -> bookShop books'
    "request" -> request books'
            
request :: (Int :< t, String :< t, [Int] :< t) => [Int] -> Spec t ()
request books = do
  get $ permutationOf books

  choice <- choose ["another", "done"]
  case choice of
    "another" -> bookShop books
    "done"    -> stop

main :: IO ()
main = do
  checkCoherence (bookShop [])
  erlangMain "bookShop:main" (bookShop [])
