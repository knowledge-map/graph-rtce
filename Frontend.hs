{-# LANGUAGE EmptyDataDecls #-}
module Frontend where

import Prelude
import DOM
import FFI
import Types

main :: Fay ()
main = addEvent "load" $ do
    conn <- openWebSocket "localhost:8000"

    conn `onMessage` \msg -> do
        str <- getData msg
        putStrLn str

    conn `onOpen` do
        let command = Create { createNode = Node { nodeId = 5, nodeContent = "hi" } }
         in conn `sendWebSocket` show command

data WebSocket
data WebSocketMessage

openWebSocket :: String -> Fay WebSocket
openWebSocket = ffi "new WebSocket('ws://'+%1)"

onMessage :: WebSocket -> (WebSocketMessage -> Fay ()) -> Fay ()
onMessage = ffi "%1['onmessage'] = %2"

onOpen :: WebSocket -> Fay () -> Fay ()
onOpen = ffi "%1['onopen'] = %2"

getData :: WebSocketMessage -> Fay String
getData = ffi "%1['data']"

sendWebSocket :: WebSocket -> String -> Fay ()
sendWebSocket = ffi "%1['send'](%2)"
