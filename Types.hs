{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Data

type ID = Int
type Client = Int

data Node = Node { nodeId :: ID, nodeContent :: String }
    deriving (Eq, Show, Read, Data, Typeable)

data Edit
    = Create { createNode :: Node }
    | UpdateID { oldId :: ID, newId :: ID }
    | UpdateContent { updateId :: ID, oldContent :: String, newContent :: String }
    | Select { selectingClient :: Client, selectingId :: Maybe ID }
    deriving (Eq, Show, Read, Data, Typeable)
