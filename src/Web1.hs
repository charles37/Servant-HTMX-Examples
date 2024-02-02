{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators     #-}

module Web1 
    ( startApp
    , app
    ) where

import           Data.Maybe
                 (fromMaybe)
import           Network.Wai
                 (Application)
import           Servant

import Text.Hamlet

import Servant.HTML.Blaze

import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp
 
import Network.Wai (Middleware,rawPathInfo, Request (requestMethod), Response, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)

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

type API = Get '[HTML] Html :<|> "clicked" :> Post '[HTML] Html 

api :: Proxy API
api = Proxy

server :: Server API
server = return [shamlet| 
    <html lang="en">
        <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Document
        <body>
        <script src="https://unpkg.com/htmx.org@1.9.0"></script>
        <!-- have a button POST a click via AJAX -->
        <button hx-post="/clicked" hx-swap="outerHTML">
            Click Me 
            |] :<|> clicked
            where
                clicked = return [shamlet|
                    <p>Clicked
                |]
    

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
    let logApp = loggingMiddleware app 
    Warp.run 8080 logApp 


--type API = "string" :> Get '[HTML] String
--    :<|> "nested" :> "html" :> Get '[HTML] (Html ())
--
--api :: Proxy API
--api = Proxy
--
--apiLink_
--    :: (IsElem endpoint API, HasLink endpoint)
--    => Proxy endpoint -> MkLink endpoint Attribute
--apiLink_ = safeAbsHref_ (Proxy :: Proxy API)
--
--
--stringLink_ :: Attribute
--stringLink_ = apiLink_ (Proxy :: Proxy ("string" :> Get '[HTML] String))
--
--
--server :: Server API
--server = return "example" :<|> return html where
--    html :: Html ()
--    html = do
--        p_ $ b_ "bar"
--        p_ $ i_ "iik"
--        p_ $ a_ [stringLink_] "string"
--
--startApp :: IO ()
--startApp = Warp.run 8080 app
--
--app :: Application
--app = serve api server
