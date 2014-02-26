{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
    (runSettings,
     defaultSettings, settingsPort, settingsIntercept)
import Web.Scotty (scottyApp, get, middleware, setHeader)
import qualified Web.Scotty as S

import Network.WebSockets
    (DataMessage(..), PendingConnection, acceptRequest,
     receiveData, sendTextData)

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Handler.WebSockets (intercept)

import Text.Blaze.Html5 hiding (head)
import Text.Blaze.Html.Renderer.Text

import Text.Read (readMaybe)

import Control.Monad (void, when, forever)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, putMVar, modifyMVar_)

import qualified Data.Map as M
import Data.Text (pack, unpack)

import Types
type History = [Edit]
type Graph = M.Map ID Node

main :: IO ()
main = do
    graphRef <- newMVar M.empty
    clientsRef <- newMVar []
    let settings = defaultSettings {
        settingsPort = 8000,
        settingsIntercept = intercept (handleConnection clientsRef graphRef)
     }
    putStrLn "Serving on http://localhost:8000"
    app' <- app
    runSettings settings app'

app :: IO Application
app = scottyApp $ do
    middleware logStdoutDev
    middleware (gzip def)
    get "/" $ blaze $ h1 "Beam me up, Scotty!"
    get "/thing.js" $ js "alert('hello!');"
    where
        blaze = S.html . renderHtml
        js  content = S.text content >> setHeader "content-type" "text/javascript"
        css content = S.text content >> setHeader "content-type" "text/css"

handleConnection clientsRef graphRef pending = do
    connection <- acceptRequest pending
    modifyMVar_ clientsRef (\cs -> return $ connection:cs)
    forever $ do
        msg <- fmap unpack $ receiveData connection
        case readMaybe msg of
            Just edit -> handleClientEvent connection clientsRef graphRef edit
            Nothing -> return ()

-- Create a new node, and inform the client if there was an ID collision.
handleClientEvent client clientsRef graphRef (Create node@(Node id' content)) = do
    graph <- takeMVar graphRef
    (graph', node') <- case M.lookup id' graph of
        Just _  -> do
            let k = largestKeyIn graph + 1
                node' = Node k content
            sendTo client $ UpdateID id' k
            return $! (M.insert k node' graph, node')
        Nothing -> return $! (M.insert id' node graph, node)
    putMVar graphRef graph'
    broadcast clientsRef $ Create node'
    where
        largestKeyIn = fst . M.findMax -- Throws error for empty map.

-- Update the node with the given id.
handleClientEvent _client clientsRef graphRef e@(UpdateContent id' old new) = do
    graph <- takeMVar graphRef
    (updated, graph') <- case M.lookup id' graph of
        Nothing -> return (False, graph)
        Just (Node _ old') -> return $ if old /= old'
            then (False, graph)
            else (True, M.adjust (\n -> n { nodeContent = new }) id' graph)
    putMVar graphRef graph'
    when updated $ broadcast clientsRef e

-- Clients should not send other event types.
handleClientEvent _ _ _ _ = return ()

sendTo client = sendTextData client . pack . show

broadcast ref e = do
    r <- readMVar ref
    mapM_ (\cl -> sendTo cl e) r
