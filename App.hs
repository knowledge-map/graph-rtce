{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
    (runSettings,
     defaultSettings, settingsPort, settingsIntercept)
import Web.Scotty (scottyApp, get, html, middleware)

import Network.WebSockets
    (DataMessage(..), acceptRequest,
     receiveDataMessage, sendTextData)

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Handler.WebSockets (intercept)

import Control.Monad (forever)
import Data.Monoid (mconcat)

app :: IO Application
app = scottyApp $ do
    middleware logStdoutDev
    middleware (gzip def)
    get "/" $ html $ mconcat ["<h1>Scotty, ", "beam", " me up!</h1>"]

handleConnection pending = do
    connection <- acceptRequest pending
    forever $ do
        msg <- receiveDataMessage connection
        print msg
        case msg of
            Text t -> sendTextData connection t
            _else  -> return ()

handleApp :: (Application -> IO ()) -> IO ()
handleApp handler = do
    app' <- app
    handler app'

main :: IO ()
main = handleApp (runSettings settings)
    where settings = defaultSettings {
        settingsPort = 8000,
        settingsIntercept = intercept handleConnection
     }
