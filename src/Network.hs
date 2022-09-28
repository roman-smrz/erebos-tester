module Network (
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
) where

import Control.Arrow
import Control.Concurrent

import Data.Text (Text)
import Data.Text qualified as T

import Process
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

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname


instance ExprType Node where
    textExprType _ = T.pack "node"
    textExprValue n = T.pack "n:" <> textNodeName (nodeName n)
    emptyVarValue = Node (NodeName T.empty) T.empty undefined undefined

    recordMembers = map (first T.pack)
        [ ("ip", RecordSelector $ nodeIp)
        ]
