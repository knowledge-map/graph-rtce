{-# LANGUAGE EmptyDataDecls #-}
module Frontend (main) where

import Prelude hiding (id)
import DOM
import FFI
import Types

main :: Fay ()
main = addEvent "load" $ do
    ws <- newWebSocket "localhost:8000"
    ws `onMessage` \msg -> getData msg >>= getEdit >>= incoming ws

    adder <- getElementById "add"
    adder `addEventListener` "mousedown" $ \e -> do
        stopProp e
        ws `sendEdit` Create {
            createNode = Node {
                nodeId = 5,
                nodeContent = "boo"
             }
         }

    body <- getBody
    body `addEventListener` "mousedown" $ \_ -> do
        ws `sendEdit` Select { selectingClient = 0, selectingId = Nothing }
        putStrLn "Clicked background"

incoming :: WebSocket -> Edit -> Fay ()
incoming ws (Create (Node id content)) = do
    el <- createElement "p"
    el `setElementId` (show id)
    el `setInnerHtml` content
    el `editable` ws
    body <- getBody
    appendChild body el

incoming _ (Select client id) = case id of
    Nothing -> do
        els <- getElementsByClassName "selected"
        mapM_ (`removeClass` "selected") els
        putStrLn "Deselect"
    Just id' -> do
        el <- getElementById (show id')
        el `addClass` "selected"
        putStrLn "Select"

incoming _ _ = return ()

editable :: Element -> WebSocket -> Fay ()
editable el ws = el `addEventListener` "mousedown" $ \e -> do
    stopProp e
    id <- getElementId el
    ws `sendEdit` Select { selectingClient = 0, selectingId = Just (readInt id) }
    putStrLn "Selecting"

readInt :: String -> Int
readInt = ffi "+%1"

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "%1['innerHTML'] = %2"

setElementId :: Element -> String -> Fay ()
setElementId = ffi "%1['id'] = %2"

getElementId :: Element -> Fay String
getElementId = ffi "%1['id']"

getEdit :: String -> Fay Edit
getEdit = ffi "JSON['parse'](%1)"

addEventListener :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3)"

getElementsByClassName :: String -> Fay [Element]
getElementsByClassName = ffi "document['getElementsByClassName'](%1)"

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

sendEdit :: WebSocket -> Edit -> Fay ()
sendEdit ws e = ws `sendData` show e
