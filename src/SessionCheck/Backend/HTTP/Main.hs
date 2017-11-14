module SessionCheck.Backend.HTTP.Main where

import Data.ByteString.Char8 (unpack, pack)
import Network.HTTP.Client
import Data.CaseInsensitive

import SessionCheck.Backend.HTTP.Types hiding (requestHeaders, requestBody)
import qualified SessionCheck.Backend.HTTP.Types as T
import SessionCheck.Backend.HTTP.Instances

type Host = String

issueRequest :: IsHTTPBody a
             => Manager
             -> Host
             -> HTTPRequest a
             -> IO ()
issueRequest manager host req = do
  initRequest <- parseRequest $ host ++ "/" ++ requestUrl req
  let request = initRequest { method         = pack $ show (requestMethod req)
                            , requestHeaders = [ (mk (pack n), pack v) | (n, v) <- T.requestHeaders req ]
                            , requestBody    = RequestBodyBS . pack . body . T.requestBody $ req
                            }
  return ()
