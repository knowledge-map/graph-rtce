{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsIntercept)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
    (scottyApp, get, middleware, setHeader, html, file)

import Network.Wai.Handler.WebSockets (intercept)
import Network.WebSockets
    (DataMessage(..), PendingConnection, ConnectionException(..), acceptRequest,
     receiveData, sendTextData)

import Control.Monad (void, when, forever)
import Control.Monad.Trans (liftIO)
import Control.Exception (handle, fromException)
import Control.Concurrent.MVar
    (MVar, newMVar, readMVar, takeMVar, putMVar, modifyMVar_)

import qualified Data.Map as M
import Data.Text (pack, unpack)

import Data.Aeson (encode, decode)
import Fay.Convert (showToFay, readFromFay)

import Pages.Index

import Types
type History = [Edit]
type Graph = M.Map ID Node

main :: IO ()
main = do
    putStrLn "Serving on http://localhost:8000"
    graphRef <- newMVar M.empty
    clientsRef <- newMVar []
    let settings = defaultSettings {
        settingsPort = 8000,
        settingsIntercept = intercept (handleConnection clientsRef graphRef)
     }
    app >>= runSettings settings

app :: IO Application
app = scottyApp $ do
    middleware logStdoutDev
    middleware (gzip def)

    get "/frontend.js" $ js "Frontend.js"
    get "/" $ blaze Pages.Index.render
    where
        blaze = html . renderHtml
        js  f = file f >> setHeader "content-type" "text/javascript"
        css f = file f >> setHeader "content-type" "text/css"

handleConnection clientsRef graphRef pending = do
    connection <- acceptRequest pending
    modifyMVar_ clientsRef (\cs -> return $ connection:cs)
    handle (catchDisconnect connection) $ forever $ do
        msg <- receiveData connection
        case fromFay msg of
            Just edit -> handleClientEvent connection clientsRef graphRef edit
            Nothing -> return ()
    where
        fromFay x = case decode x of
            Just x' -> readFromFay x'
            Nothing -> Nothing
        catchDisconnect client e = case fromException e of
            Just ConnectionClosed -> liftIO $ do
                putStrLn "Client disconnected"
                modifyMVar_ clientsRef $ return . filter (/= client)

-- Create a new node, and inform the client if there was an ID collision.
handleClientEvent client clientsRef graphRef (Create node@(Node id content)) = do
    graph <- takeMVar graphRef
    (node', graph') <- case M.lookup id graph of
        Nothing -> return (node, M.insert id node graph)
        Just _  -> do
            let id' = (fst . M.findMax $ graph) + 1
                node' = Node id' content
            sendTo client $ UpdateID id id'
            return (node', M.insert id' node' graph)
    putMVar graphRef graph'
    broadcast clientsRef $ Create node'

-- Update the node with the given id.
handleClientEvent _client clientsRef graphRef edit@(UpdateContent id old new) = do
    graph <- takeMVar graphRef
    (updated, graph') <- case M.lookup id graph of
        Nothing -> return (False, graph)
        Just (Node _ old') -> return $ if old /= old'
            then (False, graph)
            else (True, M.adjust (\n -> n { nodeContent = new }) id graph)
    putMVar graphRef graph'
    when updated $ broadcast clientsRef edit

-- If someone selects a node, let everyone know to update their UI state.
handleClientEvent _ clientsRef _ edit = broadcast clientsRef edit

-- Clients should not send other event types.
handleClientEvent _ _ _ _ = return ()

sendTo client = sendTextData client . encode . showToFay

broadcast ref e = do
    r <- readMVar ref
    mapM_ (\cl -> sendTo cl e) r
