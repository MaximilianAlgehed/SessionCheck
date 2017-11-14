{-# LANGUAGE DataKinds, TypeOperators #-}
module Main where

import Servant
import Servant.HTML.Lucid
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans

type API = "hello" :> Get '[HTML] String

api :: Proxy API
api = Proxy

server :: Server API
server = do
  liftIO $ putStrLn "Request!"
  return "hello"

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
