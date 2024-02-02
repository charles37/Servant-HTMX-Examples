{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Web1 
    ( startApp
    , app
    ) where

import           Servant

import Text.Hamlet
import Servant.HTML.Blaze
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware,rawPathInfo, Request (requestMethod))
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics (Generic)
import Control.Concurrent.STM
import Web.FormUrlEncoded

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

type AddContactWithHeader = "contacts" :> ReqBody '[FormUrlEncoded] Contact :> Post '[HTML] (Headers '[Header "HX-Trigger" String] Html)
type ContactsTableRoute = "contacts" :> "table" :> Get '[HTML] Html
type HomepageRoute = Get '[HTML] Html

type API = HomepageRoute :<|> ContactsTableRoute :<|> AddContactWithHeader 

data Contact = Contact
  { name :: ~String
  , email :: ~String
  } deriving (Eq, Show, Generic)

instance FromForm Contact
instance FromJSON Contact

type AppState = TVar [Contact]

appServer :: AppState -> Server API
appServer appState = (homepage appState) :<|> (contactsTableHandler appState) :<|> (createContactHandler appState) 

contactsTableHTML :: [Contact] -> Html
contactsTableHTML contacts = [shamlet|
    <tbody id="contacts-table" hx-get="/contacts/table" hx-trigger="newContact from:body">
        $forall contact <- contacts
            <tr>
                <td>#{name contact}
                <td>#{email contact}
    |]


-- | Get the contacts table
contactsTableHandler :: AppState -> Handler Html
contactsTableHandler appState = do
    contacts <- liftIO $ readTVarIO appState 
    return $ contactsTableHTML contacts

-- | Create a new contact and add it to the list of contacts
-- | When a successful contact creation occurs during a POST to /contacts, the response includes an HX-Trigger response header that looks like this:
-- | HX-Trigger:newContact
createContactHandler :: AppState -> Contact -> Handler (Headers '[Header "HX-Trigger" String] Html)
createContactHandler appState contact = do
    liftIO $ atomically $ do
        contacts <- readTVar appState
        writeTVar appState (contact : contacts)
    let htmlContent = [shamlet|
        <form hx-post="/contacts">
          <label>
            Name
            <input name="name" type="text">  
          <label>
            Email
            <input name="email" type="email">  
          <button type="submit">Submit

        |]
    return $ addHeader "newContact" htmlContent 

createContactFormHTML :: Html
createContactFormHTML = [shamlet|
    <form hx-post="/contacts">
      <label>
        Name
        <input name="name" type="text">  
      <label>
        Email
        <input name="email" type="email">  
      <button type="submit">Submit
    |]

api :: Proxy API
api = Proxy

homepage :: AppState -> Handler Html
homepage appState = do
    contacts <- liftIO $ readTVarIO appState 
    return [shamlet|
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        #{cssStyle}
        <title>Document
    <body>
        <script src="https://unpkg.com/htmx.org@1.9.0"></script>
        <h2>Contacts
        <table class="table">
          <thead>
            <tr>
              <th>Name
              <th>Email
              <th>
              #{contactsTableHTML contacts}
        <h2>Add A Contact
        #{createContactFormHTML}
    |]


app :: AppState -> Application
app contacts = serve api (appServer contacts)


startApp :: IO ()
startApp = do
    initContacts <- newTVarIO [Contact "Alice" "alice@Test.com"]
    let logApp = loggingMiddleware (app initContacts)
    Warp.run 8080 logApp 


cssStyle :: Html 
cssStyle = [shamlet|
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 0;
            background-color: #f4f4f4;
        }
        .container {
            width: 80%;
            margin: auto;
            overflow: hidden;
        }
        h2 {
            color: #333;
            margin-top: 1em;
        }
        .table {
            width: 100%;
            border-collapse: collapse;
        }
        .table, .table th, .table td {
            border: 1px solid #ddd;
        }
        .table th, .table td {
            padding: 10px;
            text-align: left;
        }
        .table th {
            background-color: #f8f8f8;
        }
        .table tr:nth-child(even) {
            background-color: #f2f2f2;
        }
        form {
            background-color: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
        }
        label {
            display: block;
            margin-bottom: 10px;
        }
        input[type="text"], input[type="email"] {
            width: 100%;
            padding: 8px;
            margin: 8px 0;
            display: inline-block;
            border: 1px solid #ccc;
            border-radius: 4px;
            box-sizing: border-box;
        }
        button[type="submit"] {
            width: 100%;
            background-color: #4CAF50;
            color: white;
            padding: 14px 20px;
            margin: 8px 0;
            border: none;
            border-radius: 4px;
            cursor: pointer;
        }
        button[type="submit"]:hover {
            background-color: #45a04
    |]



