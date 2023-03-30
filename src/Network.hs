module Network (
    Internet(..),
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
    nextNodeName,

    HasNetns(..),
    callOn,

    newInternet, delInternet,
    newSubnet,
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

import Network.Ip
import Test

{-
NETWORK STRUCTURE
=================

Local network (namespace "s<PREFIX>", e.g. "s1_2"):

  (upstream, if any)                 (to subnets, if any and prefix length < 24)
         ↑                           veth_sX_1 (IP: prefix.1(.0)*.254)
       veth0                         veth_sX_2 (IP: prefix.2(.0)*.254) → veth0 in subnet namespace
         |                           veth_sX_3 (IP: prefix.3(.0)*.254)
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
    , netNodes :: TVar [Node]
    , netSubnets :: TVar [(Word8, Network)]
    , netDir :: FilePath
    }

textNetworkName :: Network -> Text
textNetworkName Network { netPrefix = IpPrefix prefix } = T.intercalate "_" (map (T.pack . show) prefix)

data Node = Node
    { nodeIp :: IpAddress
    , nodeName :: NodeName
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
    emptyVarValue = Network (IpPrefix []) undefined undefined undefined

instance ExprType Node where
    textExprType _ = T.pack "node"
    textExprValue n = T.pack "n:" <> textNodeName (nodeName n)
    emptyVarValue = Node (IpAddress (IpPrefix []) 0) (NodeName T.empty 0) undefined undefined

    recordMembers = map (first T.pack)
        [ ("ip", RecordSelector $ textIpAddress . nodeIp)
        ]


nextPrefix :: IpPrefix -> [Word8] -> Word8
nextPrefix _ used = maximum (0 : used) + 1

newInternet :: MonadIO m => FilePath -> m Internet
newInternet dir = do
    inet <- liftIO $ atomically $ do
        Internet
            <$> pure dir
            <*> newNetwork (IpPrefix [1]) dir
    initNetwork $ inetRoot inet
    return inet

delInternet :: MonadIO m => Internet -> m ()
delInternet _ = liftIO $ do
    callCommand $ "ip -all netns delete"

newSubnet :: MonadIO m => Network -> Maybe VarName -> m Network
newSubnet net vname = do
    sub <- liftIO $ atomically $ do
        pref <- nextPrefix (netPrefix net) . map fst <$> readTVar (netSubnets net)
        sub <- newNetwork
            (ipSubnet pref (netPrefix net))
            (netDir net </> maybe (T.unpack $ netnsName net) (("sub_"++) . unpackVarName) vname)
        modifyTVar (netSubnets net) ((pref, sub) :)
        return sub
    initNetwork sub

    let lan = lanSubnet $ netPrefix sub
        lanIp = IpAddress lan
        bridge = lanIp 1
        router = lanIp 254

    liftIO $ do
        callOn net $ "ip link add \"veth_" <> netnsName sub <> "\" type veth peer name veth0 netns \"" <> netnsName sub <> "\""
        callOn net $ "ip addr add dev \"veth_" <> netnsName sub <> "\" " <> textIpAddressCidr router
        callOn net $ "ip link set dev \"veth_" <> netnsName sub <> "\" up"

        -- If the new subnet can be split further, routing rule for the whole prefix is needed
        when (allowsSubnets (netPrefix sub)) $ callOn net $ "ip route add "
            <> textIpNetwork (netPrefix sub)
            <> " via " <> textIpAddress bridge
            <> " dev \"veth_" <> netnsName sub <> "\""
            <> " src " <> textIpAddress router

        callOn sub $ "ip link set dev veth0 master br0 up"
        callOn sub $ "ip route add default via " <> textIpAddress router <> " dev br0 src " <> textIpAddress bridge
    return sub

newNetwork :: IpPrefix -> FilePath -> STM Network
newNetwork prefix dir = do
    Network
        <$> pure prefix
        <*> newTVar []
        <*> newTVar []
        <*> pure dir

initNetwork :: MonadIO m => Network -> m ()
initNetwork net = liftIO $ do
    let lan = lanSubnet $ netPrefix net
        lanIp = IpAddress lan
    createDirectoryIfMissing True $ netDir net
    callCommand $ T.unpack $ "ip netns add \"" <> netnsName net <> "\""
    callOn net $ "ip link add name br0 type bridge"
    callOn net $ "ip addr add " <> textIpAddressCidr (lanIp 1) <> " broadcast " <> textIpAddress (lanIp 255) <> " dev br0"
    callOn net $ "ip link set dev br0 up"
    callOn net $ "ip link set dev lo up"

newNode :: MonadIO m => Network -> VarName -> m Node
newNode net vname = liftIO $ do
    let lan = lanSubnet $ netPrefix net
        lanIp = IpAddress lan

    (node, idx) <- atomically $ do
        nodes <- readTVar (netNodes net)
        let nname = nextNodeName vname $ map nodeName nodes
            idx = fromIntegral $ 2 + length nodes
            node = Node { nodeName = nname
                        , nodeIp = lanIp idx
                        , nodeNetwork = net
                        , nodeDir = netDir net </> ("node_" ++ unpackNodeName nname)
                        }
        writeTVar (netNodes net) (node : nodes)
        return (node, idx)

    let dir = nodeDir node
    exists <- doesPathExist dir
    when exists $ ioError $ userError $ dir ++ " exists"
    createDirectoryIfMissing True dir

    let veth = T.pack $ "veth" <> show idx
    callCommand $ T.unpack $ "ip netns add \"" <> netnsName node <> "\""
    callOn net  $ "ip link add " <> veth <> " type veth peer name veth0 netns \"" <> netnsName node <> "\""
    callOn net  $ "ip link set dev " <> veth <> " master br0 up"
    callOn node $ "ip addr add " <> textIpAddressCidr (nodeIp node) <> " broadcast " <> textIpAddress (lanIp 255) <> " dev veth0"
    callOn node $ "ip link set dev veth0 up"
    callOn node $ "ip link set dev lo up"
    callOn node $ "ip route add default via " <> textIpAddress (lanIp 1) <> " dev veth0 src " <> textIpAddress (nodeIp node)

    return node
