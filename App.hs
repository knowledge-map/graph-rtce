{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)

import Network.Wai
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsIntercept)
import Web.Scotty
    (scottyApp, get, middleware, setHeader)
import qualified Web.Scotty as S

import Network.WebSockets
    (DataMessage(..), PendingConnection, acceptRequest,
     receiveData, sendTextData)

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Handler.WebSockets (intercept)

import Text.Blaze.Html5 hiding (head)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Control.Monad (void, when, forever)
import Control.Concurrent.MVar
    (MVar, newMVar, readMVar, takeMVar, putMVar, modifyMVar_)

import qualified Data.Map as M
import Data.Text (pack, unpack)

import Data.Aeson (encode, decode)
import Fay.Convert (showToFay, readFromFay)

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
    get "/" $ blaze $ do
        H.head $ H.script "" ! A.src "frontend.js"
        H.body $ do
            H.h1 "Beam me up, Scotty!"
            H.a  "Add node" ! A.id "add"
    where
        blaze = S.html . renderHtml
        js  file = S.file file >> setHeader "content-type" "text/javascript"
        css file = S.file file >> setHeader "content-type" "text/css"

handleConnection clientsRef graphRef pending = do
    connection <- acceptRequest pending
    modifyMVar_ clientsRef (\cs -> return $ connection:cs)
    forever $ do
        msg <- receiveData connection
        case fromFay msg of
            Just edit -> handleClientEvent connection clientsRef graphRef edit
            Nothing -> return ()
    where
        fromFay x = case decode x of
            Just x' -> readFromFay x'
            Nothing -> Nothing

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

-- Clients should not send other event types.
handleClientEvent _ _ _ _ = return ()

sendTo client = sendTextData client . encode . showToFay

broadcast ref e = do
    r <- readMVar ref
    mapM_ (\cl -> sendTo cl e) r
