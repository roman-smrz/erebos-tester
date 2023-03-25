module Network (
    Internet(..),
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
    nextNodeName,

    HasNetns(..),
    callOn,

    newInternet, delInternet,
    newNode,
) where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word

import System.Directory
import System.FilePath
import System.Process

import Test

data Internet = Internet
    { inetDir :: FilePath
    , inetRoot :: Network
    }

data Network = Network
    { netPrefix :: [Word8]
    , netNodes :: TVar [Node]
    , netDir :: FilePath
    }

textNetworkName :: Network -> Text
textNetworkName n = T.intercalate "_" (map (T.pack . show) (netPrefix n))

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


class HasNetns a where netnsName :: a -> Text
instance HasNetns Network where netnsName n = "s" <> textNetworkName n
instance HasNetns Node where netnsName n = netnsName (nodeNetwork n) <> ":" <> textNodeName (nodeName n)

callOn :: HasNetns a => a -> Text -> IO ()
callOn n cmd = callCommand $ T.unpack $ "ip netns exec \"" <> netnsName n <> "\" " <> cmd


instance ExprType Network where
    textExprType _ = T.pack "network"
    textExprValue n = "s:" <> textNetworkName n
    emptyVarValue = Network [] undefined undefined

instance ExprType Node where
    textExprType _ = T.pack "node"
    textExprValue n = T.pack "n:" <> textNodeName (nodeName n)
    emptyVarValue = Node (NodeName T.empty 0) T.empty undefined undefined

    recordMembers = map (first T.pack)
        [ ("ip", RecordSelector $ nodeIp)
        ]


makeIpAddress :: [Word8] -> Word8 -> Text
makeIpAddress prefix num = T.intercalate "." $ map (T.pack . show) $ prefix ++ replicate (3 - length prefix) 0 ++ [num]

newInternet :: MonadIO m => FilePath -> m Internet
newInternet dir = do
    inet <- liftIO $ atomically $ do
        Internet
            <$> pure dir
            <*> newNetwork [1] dir
    initNetwork $ inetRoot inet
    return inet

delInternet :: MonadIO m => Internet -> m ()
delInternet _ = liftIO $ do
    callCommand $ "ip -all netns delete"

newNetwork :: [Word8] -> FilePath -> STM Network
newNetwork prefix dir = do
    Network
        <$> pure prefix
        <*> newTVar []
        <*> pure dir

initNetwork :: MonadIO m => Network -> m ()
initNetwork net = liftIO $ do
    callCommand $ T.unpack $ "ip netns add \"" <> netnsName net <> "\""
    callOn net $ "ip link add name br0 type bridge"
    callOn net $ "ip addr add " <> makeIpAddress (netPrefix net) 1 <> " broadcast " <> makeIpAddress (netPrefix net) 255 <> " dev br0"
    callOn net $ "ip link set dev br0 up"
    callOn net $ "ip link set dev lo up"

newNode :: MonadIO m => Network -> VarName -> m Node
newNode net vname = liftIO $ do
    node <- atomically $ do
        nodes <- readTVar (netNodes net)
        let nname = nextNodeName vname $ map nodeName nodes
            node = Node { nodeName = nname
                        , nodeIp = makeIpAddress (netPrefix net) (fromIntegral $ 2 + length nodes)
                        , nodeNetwork = net
                        , nodeDir = netDir net </> ("node_" ++ unpackNodeName nname)
                        }
        writeTVar (netNodes net) (node : nodes)
        return node

    let name = textNodeName $ nodeName node
        dir = nodeDir node

    exists <- doesPathExist dir
    when exists $ ioError $ userError $ dir ++ " exists"
    createDirectoryIfMissing True dir

    callCommand $ T.unpack $ "ip netns add \"" <> netnsName node <> "\""
    callOn net  $ "ip link add \"veth_" <> name <> "\" type veth peer name veth0 netns \"" <> netnsName node <> "\""
    callOn net  $ "ip link set dev \"veth_" <> name <> "\" master br0 up"
    callOn node $ "ip addr add " <> nodeIp node <> "/24 broadcast " <> makeIpAddress (netPrefix net) 255 <> " dev veth0"
    callOn node $ "ip link set dev veth0 up"
    callOn node $ "ip link set dev lo up"

    return node
