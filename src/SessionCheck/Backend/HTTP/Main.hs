module SessionCheck.Backend.HTTP.Main where

import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Client hiding (host, port)
import Network.HTTP.Types.Status
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp hiding (run)
import qualified Network.Wai.Handler.Warp as Warp
import Data.CaseInsensitive hiding (map)
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import System.Timeout
import Data.Char
import Control.Exception
import System.Process

import SessionCheck.Backend.HTTP.Types hiding (requestHeaders, requestBody)
import qualified SessionCheck.Backend.HTTP.Types as T
import SessionCheck.Backend.HTTP.Instances
import SessionCheck.Backend
import SessionCheck.Spec
import SessionCheck.Types
import SessionCheck.Test

type Host = String

issueRequest :: Manager
             -> Host
             -> HTTPRequest String
             -> IO (HTTPReply String)
issueRequest manager host req = do
  initRequest <- parseRequest $ host ++ "/" ++ requestUrl req
  let request = initRequest { method         = pack $ show (requestMethod req)
                            , requestHeaders = [ (mk (pack n), pack v) | (n, v) <- T.requestHeaders req ]
                            , requestBody    = RequestBodyBS . pack . T.requestBody $ req
                            }
  response <- (fmap L.unpack) <$> httpLbs request manager
  return $ HTTPReply { replyBody    = responseBody response
                     , replyStatus  = StatusCode (statusCode $ responseStatus response)
                     , replyHeaders = [ (unpack (original n), unpack v) | (n, v) <- responseHeaders response ] }

data Options = Options { host    :: Host
                       , port    :: Port
                       , program :: String }

defaultOptions :: Options
defaultOptions = Options { host    = "http://localhost"
                         , port    = 8081
                         , program = "echo \"Hi\"" }

runClient :: Options
          -> Implementation HTTPMessage
          -> IO ()
runClient opts imp = do
  manager <- newManager defaultManagerSettings
  loop manager
  putMVar (done imp) ()
  where
    loop manager = do
      d <- isDead imp
      unless d $ do
        mr <- timeout 1000 $ atomically $ readTChan (outputChan imp)
        case mr of
          Just (Request r) -> do
            reply <- issueRequest manager (host opts ++ ":" ++ show (port opts)) r
            atomically $ writeTChan (inputChan imp) (Reply reply)
            loop manager 
          Just (Reply _) -> kill imp (Bad "Specification does not follow request-reply structure")
          _ -> loop manager

runServer :: Options
          -> Implementation HTTPMessage
          -> IO ()
runServer opts imp = undefined

httpMain :: ProtocolRole
         -> Options
         -> Spec HTTPMessage a -- Specification
         -> IO ()
httpMain r opts spec = case r of
  Server -> do
    imp <- clean
    tid <- forkIO $ Warp.run (port opts) (runfun imp)
    sessionCheck (imp { run = void $ spawnCommand (program opts ++ " > /dev/null")  }) spec
    killThread tid
    where
      runfun imp req cont = do
        body <- Wai.strictRequestBody req
        atomically . writeTChan (inputChan imp) . Request $ HTTPRequest (read . map toUpper . unpack . Wai.requestMethod $ req)
                                                                        (unpack . Wai.rawPathInfo $ req)
                                                                        ([(map toLower $ unpack (original n), unpack v) | (n, v) <- Wai.requestHeaders req])
                                                                        (L.unpack body)
        d <- isDead imp
        when d $ throwIO (userError "Something went wrong!")
        m <- atomically $ readTChan (outputChan imp)
        r <- case m of
              Reply r -> return $ Wai.responseLBS (mkStatus (let StatusCode s = replyStatus r in s) mempty)
                                                  [(mk (pack n), pack v) | (n, v) <- replyHeaders r]
                                                  (L.pack (replyBody r))
              _ -> do
                kill imp (Bad "Specification does not follow request-reply structure")
                throwIO $ userError "Something went wrong!"
        cont r
  Client -> do
    ph <- spawnCommand $ program opts ++ " > /dev/null"
    imp <- clean 
    sessionCheck (imp { run = runClient opts imp }) spec
    terminateProcess ph
