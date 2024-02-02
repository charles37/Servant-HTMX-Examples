{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where

import           Data.Maybe
                 (fromMaybe)
import           Lucid
import           Lucid.Servant
import           Network.Wai
                 (Application)
import           Servant
import           Servant.HTML.Lucid
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

type API = "string" :> Get '[HTML] String
    :<|> "nested" :> "html" :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

apiLink_
    :: (IsElem endpoint API, HasLink endpoint)
    => Proxy endpoint -> MkLink endpoint Attribute
apiLink_ = safeAbsHref_ (Proxy :: Proxy API)

stringLink_ :: Attribute
stringLink_ = apiLink_ (Proxy :: Proxy ("string" :> Get '[HTML] String))

server :: Server API
server = return "example" :<|> return html where
    html :: Html ()
    html = do
        p_ $ b_ "bar"
        p_ $ i_ "iik"
        p_ $ a_ [stringLink_] "string"

startApp :: IO ()
startApp = Warp.run 8080 app

app :: Application
app = serve api server
