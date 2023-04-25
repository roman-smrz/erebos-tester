module Network (
    Internet(..),
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
    nextNodeName,

    newInternet, delInternet,
    newSubnet,
    newNode,
) where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word

import System.Directory
import System.FilePath
import System.Process

import Network.Ip
import Test

{-
NETWORK STRUCTURE
=================

Local network (namespace "s<PREFIX>", e.g. "s1_2"):

  (upstream, if any)                 (to subnets, if any and prefix length < 24)
         ↑                           veth_s1 (IP: prefix.1(.0)*.254)
       veth0                         veth_s2 (IP: prefix.2(.0)*.254) → veth0 in subnet namespace
         |                           veth_s3 (IP: prefix.3(.0)*.254)
        br0 (IP: prefix(.0)*.1/24)   ...
       / | \
  veth2 ... veth253
    ↓    ↓    ↓
     (to nodes)

Node (namespace "s<PREFIX>:<NODE>", e.g. "s1_2:p0"):

     (upstream)
         ↑
       veth0 (IP: prefix.N/24)
-}

data Internet = Internet
    { inetDir :: FilePath
    , inetRoot :: Network
    }

data Network = Network
    { netPrefix :: IpPrefix
    , netNetns :: NetworkNamespace
    , netBridge :: Link Bridge
    , netNodes :: TVar [Node]
    , netSubnets :: TVar [(Word8, Network)]
    , netDir :: FilePath
    }

textNetworkName :: IpPrefix -> Text
textNetworkName (IpPrefix prefix) = T.intercalate "_" (map (T.pack . show) prefix)

data Node = Node
    { nodeIp :: IpAddress
    , nodeName :: NodeName
    , nodeNetns :: NetworkNamespace
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


instance HasNetns Network where getNetns = netNetns
instance HasNetns Node where getNetns = nodeNetns

instance ExprType Network where
    textExprType _ = T.pack "network"
    textExprValue n = "s:" <> textNetworkName (netPrefix n)
    emptyVarValue = Network (IpPrefix []) undefined undefined undefined undefined undefined

instance ExprType Node where
    textExprType _ = T.pack "node"
    textExprValue n = T.pack "n:" <> textNodeName (nodeName n)
    emptyVarValue = Node (IpAddress (IpPrefix []) 0) (NodeName T.empty 0) undefined undefined undefined

    recordMembers = map (first T.pack)
        [ ("ip", RecordSelector $ textIpAddress . nodeIp)
        ]


nextPrefix :: IpPrefix -> [Word8] -> Word8
nextPrefix _ used = maximum (0 : used) + 1

newInternet :: MonadIO m => FilePath -> m Internet
newInternet dir = do
    atomicallyWithIO $ do
        Internet
            <$> pure dir
            <*> newNetwork (IpPrefix [1]) dir

delInternet :: MonadIO m => Internet -> m ()
delInternet _ = liftIO $ do
    callCommand $ "ip -all netns delete"

newSubnet :: MonadIO m => Network -> Maybe VarName -> m Network
newSubnet net vname = do
    (sub, idx) <- atomicallyWithIO $ do
        idx <- lift $ nextPrefix (netPrefix net) . map fst <$> readTVar (netSubnets net)
        sub <- newNetwork
            (ipSubnet idx (netPrefix net))
            (netDir net </> maybe (T.unpack $ textNetnsName $ getNetns net) (("sub_"++) . unpackVarName) vname)
        lift $ modifyTVar (netSubnets net) ((idx, sub) :)
        return (sub, idx)

    let lan = lanSubnet $ netPrefix sub
        lanIp = IpAddress lan
        bridge = lanIp 1
        router = lanIp 254

    liftIO $ do
        (vethNet, vethSub) <- addVEth (net, "veth_s" <> T.pack (show idx)) (sub, "veth0")
        addAddress vethNet router
        setMaster vethSub (netBridge sub) -- this end needs to go up first, otherwise it
        linkUp    vethSub                 -- sometimes gets stuck with NO-CARRIER for a while.
        linkUp    vethNet

        -- If the new subnet can be split further, routing rule for the whole prefix is needed
        when (allowsSubnets (netPrefix sub)) $ callOn net $ "ip route add "
            <> textIpNetwork (netPrefix sub)
            <> " via " <> textIpAddress bridge
            <> " dev " <> linkName vethNet
            <> " src " <> textIpAddress router
        callOn sub $ "ip route add default via " <> textIpAddress router <> " dev br0 src " <> textIpAddress bridge
    return sub

newNetwork :: IpPrefix -> FilePath -> WriterT [IO ()] STM Network
newNetwork prefix dir = do
    postpone $ createDirectoryIfMissing True dir

    netns <- addNetworkNamespace ("s" <> textNetworkName prefix)
    bridge <- addBridge netns "br0"

    addAddress bridge $ IpAddress (lanSubnet prefix) 1
    linkUp $ bridge
    linkUp $ loopback netns

    Network
        <$> pure prefix
        <*> pure netns
        <*> pure bridge
        <*> lift (newTVar [])
        <*> lift (newTVar [])
        <*> pure dir

newNode :: MonadIO m => Network -> VarName -> m Node
newNode net vname = liftIO $ do
    let lan = lanSubnet $ netPrefix net
        lanIp = IpAddress lan

    (node, idx) <- atomicallyWithIO $ do
        nodes <- lift $ readTVar (netNodes net)
        let nname = nextNodeName vname $ map nodeName nodes
        netns <- addNetworkNamespace $ textNetnsName (getNetns net) <> ":" <> textNodeName nname
        let idx = fromIntegral $ 2 + length nodes
            node = Node { nodeName = nname
                        , nodeNetns = netns
                        , nodeIp = lanIp idx
                        , nodeNetwork = net
                        , nodeDir = netDir net </> ("node_" ++ unpackNodeName nname)
                        }
        lift $ writeTVar (netNodes net) (node : nodes)
        return (node, idx)

    let dir = nodeDir node
    exists <- doesPathExist dir
    when exists $ ioError $ userError $ dir ++ " exists"
    createDirectoryIfMissing True dir

    (vethNet, vethNode) <- addVEth (net, "veth" <> T.pack (show idx)) (node, "veth0")
    setMaster vethNet $ netBridge net
    linkUp vethNet
    addAddress vethNode $ nodeIp node
    linkUp $ vethNode
    linkUp $ loopback node
    callOn node $ "ip route add default via " <> textIpAddress (lanIp 1) <> " dev veth0 src " <> textIpAddress (nodeIp node)

    return node

atomicallyWithIO :: MonadIO m => WriterT [IO ()] STM a -> m a
atomicallyWithIO act = liftIO $ do
    (x, fin) <- atomically $ runWriterT act
    sequence_ fin
    return x
