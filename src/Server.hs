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
    (DataMessage(..), receiveData, sendTextData,
     PendingConnection, acceptRequest, Connection, ConnectionException(..))

import Control.Monad (void, when, forever)
import Control.Monad.Trans (liftIO)
import Control.Exception (handle, fromException)
import Control.Concurrent.STM
    (STM, TVar, newTVar, readTVar, writeTVar, atomically)

import qualified Data.Map as M
import Data.Text (pack, unpack)

import Data.Aeson (encode, decode)
import Fay.Convert (showToFay, readFromFay)

import Pages.Index

import Types
type History = [Edit]
type Graph = M.Map ID Node
type Clients = M.Map ID Connection

main :: IO ()
main = do
    putStrLn "Serving on http://localhost:8000"
    graphV <- atomically $ newGraph
    clientsV <- atomically $ newClients
    let settings = defaultSettings {
        settingsPort = 8000,
        settingsIntercept = intercept (handleConnection clientsV graphV)
     }
    runSettings settings =<< app

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

{- WebSockets connection handler -}

handleConnection clientsV graphV pending = do
    connection <- acceptRequest pending
    client <- atomically $ addClient clientsV connection
    handle (catchDisconnect client) $ forever $ do
        msg <- receiveData connection
        case fromFay msg of
            Just edit -> handleClientEvent client clientsV graphV edit
            Nothing -> return ()
    where
        fromFay x = case decode x of
            Just x' -> readFromFay x'
            Nothing -> Nothing
        catchDisconnect client e = case fromException e of
            Just ConnectionClosed -> liftIO $ do
                putStrLn "Client disconnected"
                atomically $ removeClient clientsV client
            _otherwise -> return ()

{- Edit event handlers -}

-- Handle an event that creates a new node.
handleClientEvent client clientsV graphV (Create node@(Node id content)) = do
    node'@(Node id' content') <- atomically $ insertNode graphV node
    when (id' /= id) $ do
        conn <- atomically $ getConnection clientsV client
        case conn of
            Nothing -> return ()
            Just c  -> sendTo c (UpdateID id id')
    broadcast clientsV (Create node')

-- Handle node content update events.
handleClientEvent _client clientsV graphV edit@(UpdateContent id old new) = do
    updated <- atomically $ updateNodeContent graphV id old new
    when updated $ broadcast clientsV edit

-- All other events are just multiplexed to all clients.
handleClientEvent _ clientsV _ edit = broadcast clientsV edit

-- Convenience: send an object to a client.
sendTo conn e = sendTextData conn . encode . showToFay $ e

-- Convenience: send an object to all clients.
broadcast clientsV e = do
    conns <- atomically $ getConnections clientsV
    mapM_ (flip sendTo e) conns

{- Graph data structure and modification -}

newGraph = newTVar M.empty

insertNode :: TVar Graph -> Node -> STM Node
insertNode graphV node@(Node id content) = do
    graph <- readTVar graphV
    -- Obtain a new node, which is either identical to 'node' if its 'id' does
    -- not already exist in the graph, or modified with a new id if it does.
    node' <- case M.lookup id graph of
        Nothing -> return node
        Just _ -> let id' = (fst . M.findMax $ graph) + 1
                   in return (Node id' content)
    writeTVar graphV $ M.insert (nodeId node') node' graph
    -- Return the new node so the user can inspect whether its ID was changed.
    return node'

updateNodeContent :: TVar Graph -> ID -> String -> String -> STM Bool
updateNodeContent graphV id old new = do
    graph <- readTVar graphV
    -- Maybe modify the node, if it exists in the graph and its content matches
    -- 'old'. If the content doesn't match, the edit is outdated.
    node' <- case M.lookup id graph of
        Nothing -> return (Node id old)
        Just node@(Node _ content) ->
            if content == old
                then return (Node id new)
                else return (Node id old)
    -- Update the graph if the node was modified, and return status either way.
    if nodeContent node' /= old
        then writeTVar graphV (M.insert id node' graph) >> return True
        else return False

{- Connected client registry -}

newClients = newTVar M.empty

addClient clientsV connection = do
    clients <- readTVar clientsV
    let id = if M.null clients
                then 0
                else (fst . M.findMax $ clients) + 1
    writeTVar clientsV $ M.insert id connection clients
    return id

removeClient clientsV client = do
    clients <- readTVar clientsV
    writeTVar clientsV $ M.delete client clients

getConnection clientsV client = return Nothing

getConnections clientsV = do
    clients <- readTVar clientsV
    return (M.elems clients)
