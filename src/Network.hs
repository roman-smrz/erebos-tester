module Network (
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
) where

import Control.Concurrent

import Data.Text (Text)
import Data.Text qualified as T

import Process

data Network = Network
    { netNodes :: MVar [Node]
    , netProcesses :: MVar [Process]
    , netDir :: FilePath
    }

data Node = Node
    { nodeName :: NodeName
    , nodeNetwork :: Network
    , nodeDir :: FilePath
    }

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname
