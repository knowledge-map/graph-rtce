{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.WebSockets (runServer, acceptRequest, receiveDataMessage, DataMessage(..), sendTextData)
import Control.Monad (forever)

main = runServer "127.0.0.1" 8080 handleConnection

handleConnection pending = do
    connection <- acceptRequest pending
    forever $ do
        msg <- receiveDataMessage connection
        print msg
        case msg of
            Text t -> sendTextData connection t
            _ -> return ()
