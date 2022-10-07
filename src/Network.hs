module Network (
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
    nextNodeName,
) where

import Control.Arrow
import Control.Concurrent

import Data.Text (Text)
import Data.Text qualified as T

import {-# SOURCE #-} Process
import Test

data Network = Network
    { netNodes :: MVar [Node]
    , netProcesses :: MVar [Process]
    , netDir :: FilePath
    }

data Node = Node
    { nodeName :: NodeName
    , nodeIp :: Text
    , nodeNetwork :: Network
    , nodeDir :: FilePath
    }

data NodeName = NodeName Text Word
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name 0) = name
textNodeName (NodeName name num) = name <> T.pack "~" <> T.pack (show num)

unpackNodeName :: NodeName -> String
unpackNodeName = T.unpack . textNodeName

nextNodeName :: VarName -> [NodeName] -> NodeName
nextNodeName (VarName tname) = go 0
  where
    go n [] = NodeName tname n
    go n (NodeName tname' m : ns) | tname == tname' = go (max n m + 1) ns
                                  | otherwise       = go n ns


instance ExprType Node where
    textExprType _ = T.pack "node"
    textExprValue n = T.pack "n:" <> textNodeName (nodeName n)
    emptyVarValue = Node (NodeName T.empty 0) T.empty undefined undefined

    recordMembers = map (first T.pack)
        [ ("ip", RecordSelector $ nodeIp)
        ]
