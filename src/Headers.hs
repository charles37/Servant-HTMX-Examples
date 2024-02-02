
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Headers where


import           Data.Maybe
                 (fromMaybe, catMaybes)
import           Network.Wai
                 (Application)
import           Servant
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

import Network.Wai (Middleware,responseLBS, rawPathInfo, Request (requestMethod, requestHeaders), Response, responseStatus)
--import Network.HTTP.Types.Status (statusCode)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Servant.Server.Internal
import Servant.HTML.Blaze
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze
import qualified Network.HTTP.Types as HTTP
import Data.ByteString

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html (Html)

import qualified Data.Text.Encoding as TE
import Data.Function ((&))
import Text.Hamlet

import Data.Aeson (ToJSON, encode, toJSON, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant
import Text.Blaze.Html.Renderer.Text (renderHtml)

--Headers.hs
data HTMXRequestHeaders = HTMXRequestHeaders
  { htmxBoosted :: ~(Maybe Bool)
  , htmxCurrentURL :: ~(Maybe T.Text)
  , htmxHistoryRestoreRequest :: ~(Maybe Bool)
  , htmxPrompt :: ~(Maybe T.Text)
  , htmxRequest :: ~(Maybe Bool)
  , htmxTarget :: ~(Maybe T.Text)
  , htmxTriggerName :: ~(Maybe T.Text)
  } deriving (Eq, Show)

-- | Default empty HTMX request headers
defaultHTMXRequestHeaders :: HTMXRequestHeaders
defaultHTMXRequestHeaders = HTMXRequestHeaders
  { htmxBoosted = Nothing
  , htmxCurrentURL = Nothing
  , htmxHistoryRestoreRequest = Nothing
  , htmxPrompt = Nothing
  , htmxRequest = Nothing
  , htmxTarget = Nothing
  , htmxTriggerName = Nothing
  }

-- | Extract HTMX request headers from a Wai Request
extractHTMXRequestHeaders :: Request -> HTMXRequestHeaders
extractHTMXRequestHeaders req = HTMXRequestHeaders
  { htmxBoosted = parseBool <$> lookup "HX-Boosted" (requestHeaders req)
  , htmxCurrentURL = TE.decodeUtf8 <$> lookup "HX-Current-URL" (requestHeaders req)
  , htmxHistoryRestoreRequest = parseBool <$> lookup "HX-History-Restore-Request" (requestHeaders req)
  , htmxPrompt = TE.decodeUtf8 <$> lookup "HX-Prompt" (requestHeaders req)
  , htmxRequest = parseBool <$> lookup "HX-Request" (requestHeaders req)
  , htmxTarget = TE.decodeUtf8 <$> lookup "HX-Target" (requestHeaders req)
  , htmxTriggerName = TE.decodeUtf8 <$> lookup "HX-Trigger-Name" (requestHeaders req)
  }
  where
    parseBool :: ByteString -> Bool
    parseBool = (== "true") . TE.decodeUtf8

-- | Servant combinator to capture HTMX request headers
-- | Servant combinator to capture HTMX request headers and ensure it's an HTMX request

isHTMXRequest :: Request -> Bool
isHTMXRequest req = (Just "true" ==) $ lookup "HX-Request" $ requestHeaders req
    

data HTMX

instance (HasServer api context) => HasServer (HTMX :> api) context where
    type ServerT (HTMX :> api) m = ServerT api m
    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api) 
    route Proxy context subserver = route (Proxy :: Proxy api) context $ addHTMXCheck subserver
      where
        addHTMXCheck :: Delayed env (Server api) -> Delayed env (Server api)
        addHTMXCheck delayed = delayed `addMethodCheck` checkHTMX

        checkHTMX :: DelayedIO ()
        checkHTMX = withRequest $ \req ->
            unless (isHTMXRequest req) $ delayedFailFatal err400 { errBody = "Non-HTMX request" }


-- Simple logging middleware
loggingMiddleware :: Middleware
loggingMiddleware appFound req respondFound = do
    -- Log the request URL and method
    liftIO $ do
        currentTime <- getCurrentTime
        putStrLn $ "Time: " ++ show currentTime ++ 
                   ", Request: " ++ show (requestMethod req) ++ 
                   ", Path: " ++ show (rawPathInfo req) ++
                   ", full Request: " ++ show req
    appFound req respondFound

-- Response.hs

-- | Represents an HTMX response
data HTMXResponse = HTMXResponse
  { reswap :: ~(Maybe SwapStrategy)
  , retarget :: ~(Maybe Text)
  , triggers :: ~[Trigger]
  , body :: ~(Maybe Html)
  , statusCode :: ~(Maybe Int)
  } 


-- | Default empty HTMX response
defaultHTMXResponse :: HTMXResponse
defaultHTMXResponse = HTMXResponse
  { reswap = Nothing
  , retarget = Nothing
  , triggers = []
  , body = Nothing
  , statusCode = Nothing
  } 

-- | Represents different swap strategies in HTMX
data SwapStrategy
  = SwapInnerHTML
  | SwapOuterHTML
  -- Add more strategies as per HTMX documentation
  deriving (Eq, Show)

-- | Represents a trigger in HTMX
data Trigger = Trigger
  { eventName :: ~Text
  , eventDetail :: ~(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON Trigger where
  toJSON (Trigger name Nothing) = object ["event" .= name]
  toJSON (Trigger name (Just detail)) = object ["event" .= name, "detail" .= detail]

-- | Sets the swap strategy for the HTMX response
setReswap :: SwapStrategy -> HTMXResponse -> HTMXResponse
setReswap strategy resp = resp { reswap = Just strategy }

-- | Sets the retarget selector for the HTMX response
setRetarget :: Text -> HTMXResponse -> HTMXResponse
setRetarget selector resp = resp { retarget = Just selector }

-- | Adds a trigger to the HTMX response
addTrigger :: Trigger -> HTMXResponse -> HTMXResponse
addTrigger trigger resp = resp { triggers = trigger : triggers resp }

-- | Sets the status code for the HTMX response
setStatusCode :: Int -> HTMXResponse -> HTMXResponse
setStatusCode code resp = resp { statusCode = Just code }

-- | Converts HTMXResponse to a Servant Response
toServantResponse :: HTMXResponse -> Handler Html
toServantResponse htmxResp = do
    let headers = buildHeaders htmxResp
    let status = HTTP.Status (fromMaybe 200 $ statusCode htmxResp) ""
    let htmlbody = renderHtml $ fromMaybe "" $ body htmxResp 
    mapM_ addHeader 
    return $ responseLBS status headers html 


-- | Helper function to build HTTP headers from HTMXResponse
buildHeaders :: HTMXResponse -> [HTTP.Header]
buildHeaders HTMXResponse{reswap, retarget} = 
  catMaybes [
    fmap (\strategy -> ("HX-Swap", encodeSwapStrategy strategy)) reswap,
    fmap (\selector -> ("HX-Target", TE.encodeUtf8 selector)) retarget
  ]

encodeSwapStrategy :: SwapStrategy -> ByteString
encodeSwapStrategy SwapInnerHTML = "innerHTML"
encodeSwapStrategy SwapOuterHTML = "outerHTML"

setBody :: Html -> HTMXResponse -> HTMXResponse
setBody html resp = resp { body = Just html }



-- Example usage
exampleHTMXResponse :: HTMXResponse
exampleHTMXResponse = defaultHTMXResponse
    & setReswap SwapOuterHTML
    & setRetarget "#my-div"
    & addTrigger (Trigger "my-event" Nothing)
    & setStatusCode 201


type HTMXAPI = Get '[HTML] Html
           :<|> "clicked" :> HTMX :> Post '[HTML] Html

htmxAPI :: Proxy HTMXAPI
htmxAPI = Proxy

-- The server implementation
server :: Server HTMXAPI
server = return homePage
    :<|> clickedEndpoint
  where
    homePage :: Html
    homePage = [shamlet|
      $doctype 5
      <html>
        <head>
          <title>HTMX Example
          <script src="https://unpkg.com/htmx.org@1.9.0/dist/htmx.min.js">
        <body>
          <button hx-post="/clicked" hx-swap="outerHTML">
            Click Me
          <div #response>
            Waiting for a click...
      |]

    clickedEndpoint :: Handler Html
    clickedEndpoint = do
      let htmxResp =  
-- Main function to run the server
main :: IO ()
main = Warp.run 8080 (serve htmxAPI server)                      --data CaptureHTMXRequestHeaders 
--
--instance (HasServer api context) => HasServer (CaptureHTMXRequestHeaders :> api) context where
--  type ServerT (CaptureHTMXRequestHeaders :> api) m = HTMXRequestHeaders -> ServerT api m
--
--  hoistServerWithContext _ a b c = hoistServerWithContext (Proxy :: Proxy api) a b . c
--
--  route Proxy context subserver = route (Proxy :: Proxy api) context $ addHTMXRequestHeadersCheck subserver
--    where
--      addHTMXRequestHeadersCheck :: Delayed env (ServerT api m) -> Delayed env (ServerT api m)
--      addHTMXRequestHeadersCheck delayed = delayed `addMethodCheck` checkHTMXRequestHeaders
--
--      checkHTMXRequestHeaders :: DelayedIO () 
--      checkHTMXRequestHeaders = withRequest $ \req -> do
--        unless (isHTMXRequest req) $ delayedFailFatal err400 { errBody = "Non-HTMX request" }
--        let htmxHeaders = extractHTMXRequestHeaders req
--        return () 
