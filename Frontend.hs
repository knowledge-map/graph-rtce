{-# LANGUAGE EmptyDataDecls #-}
module Frontend where

import Prelude
import DOM
import FFI
import Types

main :: Fay ()
main = addEvent "load" $ do
    ws <- newWebSocket "localhost:8000"
    ws `onMessage` \msg -> do
        edit <- getData msg >>= getEdit
        case edit of
            Create n -> putStrLn "create"
            UpdateID a b -> putStrLn "updateid"
            UpdateContent a b c -> putStrLn "updatecontent"

getEdit :: String -> Fay Edit
getEdit = ffi "JSON.parse(%1)"

data WebSocket
data WebSocketMessage

newWebSocket :: String -> Fay WebSocket
newWebSocket = ffi "new WebSocket('ws://'+%1)"

onMessage :: WebSocket -> (WebSocketMessage -> Fay ()) -> Fay ()
onMessage = ffi "%1['onmessage'] = %2"

onOpen :: WebSocket -> Fay () -> Fay ()
onOpen = ffi "%1['onopen'] = %2"

getData :: WebSocketMessage -> Fay String
getData = ffi "%1['data']"

sendData :: WebSocket -> String -> Fay ()
sendData = ffi "%1['send'](%2)"
