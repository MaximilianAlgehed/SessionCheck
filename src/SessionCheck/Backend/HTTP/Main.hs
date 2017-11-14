module SessionCheck.Backend.HTTP.Main where

import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Client hiding (host)
import Network.HTTP.Types.Status
import Data.CaseInsensitive
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import System.Timeout

import SessionCheck.Backend.HTTP.Types hiding (requestHeaders, requestBody)
import qualified SessionCheck.Backend.HTTP.Types as T
import SessionCheck.Backend.HTTP.Instances
import SessionCheck.Backend
import SessionCheck.Spec
import SessionCheck.Types

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

data Options = Options { host :: Host }

runClient :: Options
          -> Implementation HTTPMessage
          -> IO ()
runClient opts imp = do
  manager <- newManager defaultManagerSettings
  loop manager
  where
    loop manager = do
      d <- isDead imp
      unless d $ do
        mr <- timeout 1000 $ atomically $ readTChan (outputChan imp)
        case mr of
          Just (Request r) -> do
            reply <- issueRequest manager (host opts) r
            atomically $ writeTChan (inputChan imp) (Reply reply)
            loop manager 
          Just (Reply _) -> (kill imp (Bad "Specification does not follow request-reply structure"))
          _ -> loop manager

