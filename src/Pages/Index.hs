{-# LANGUAGE OverloadedStrings #-}
module Pages.Index (render) where

import Prelude hiding (head, id)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

render = do
    head $ script "" ! src "js/app.js"
    body $ do
        h1 "Beam me up, Scotty!"
        a  "Add node" ! id "add"
