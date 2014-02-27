{-# LANGUAGE EmptyDataDecls #-}
module Frontend (main) where

import Prelude hiding (id)
import DOM
import FFI
import Types

main :: Fay ()
main = addEvent "load" $ do
    ws <- newWebSocket "localhost:8000"
    ws `onMessage` \msg -> getData msg >>= getEdit >>= incoming

    adder <- getElementById "add"
    adder `addEventListener` "mousedown" $ \_ -> do
        let n = Create { createNode = Node { nodeId = 5, nodeContent = "boo" } }
        ws `sendData` show n

incoming :: Edit -> Fay ()
incoming (Create (Node id content)) = do
    el <- createElement "p"
    setInnerHtml el (show id ++ " " ++ content)
    body <- getBody
    appendChild body el
incoming _ = return ()

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "%1['innerHTML'] = %2"

getEdit :: String -> Fay Edit
getEdit = ffi "JSON['parse'](%1)"

addEventListener :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3)"

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
