module BookShop where

import SessionCheck
import SessionCheck.Backend.Erlang

bookShop :: [Int] -> Spec ErlType ()
bookShop books = do 
  b <- send anything 
  let books' = b : books

  choice <- choose ["another", "request"]
  case choice of
    "another" -> bookShop books'
    "request" -> request books'
            
request :: [Int] -> Spec ErlType ()
request books = do
  get $ permutationOf books

  choice <- choose ["another", "done"]
  case choice of
    "another" -> bookShop books
    "done"    -> stop
