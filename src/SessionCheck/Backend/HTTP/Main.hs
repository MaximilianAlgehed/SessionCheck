module SessionCheck.Backend.HTTP.Main where

import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Data.CaseInsensitive

import SessionCheck.Backend.HTTP.Types hiding (requestHeaders, requestBody)
import qualified SessionCheck.Backend.HTTP.Types as T
import SessionCheck.Backend.HTTP.Instances

type Host = String

issueRequest :: IsHTTPBody a
             => Manager
             -> Host
             -> HTTPRequest a
             -> IO (HTTPReply String)
issueRequest manager host req = do
  initRequest <- parseRequest $ host ++ "/" ++ requestUrl req
  let request = initRequest { method         = pack $ show (requestMethod req)
                            , requestHeaders = [ (mk (pack n), pack v) | (n, v) <- T.requestHeaders req ]
                            , requestBody    = RequestBodyBS . pack . body . T.requestBody $ req
                            }
  response <- (fmap L.unpack) <$> httpLbs request manager
  return $ HTTPReply { replyBody    = responseBody response
                     , replyStatus  = StatusCode (statusCode $ responseStatus response)
                     , replyHeaders = [ (unpack (original n), unpack v) | (n, v) <- responseHeaders response ] }
