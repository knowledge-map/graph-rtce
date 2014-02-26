module Types where

type ID = Int

data Node = Node { nodeId :: ID, nodeContent :: String }
    deriving (Eq, Show, Read)

data Edit
    = Create { createNode :: Node }
    | UpdateID { oldId :: ID, newId :: ID }
    | UpdateContent { updateId :: ID, oldContent :: String, newContent :: String }
    deriving (Eq, Show, Read)
