module Network (
    Internet(..),
    Network(..),
    Node(..),
    NodeName(..), textNodeName, unpackNodeName,
    nextNodeName,

    rootNetworkVar,
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
import Script.Expr
import Script.Expr.Class

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
    , netUpstream :: Maybe (Link VEth)
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
    , nodeUpstream :: Link VEth
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
    textExprValue n = "<network:" <> textNetworkName (netPrefix n) <> ">"

instance ExprType Node where
    textExprType _ = T.pack "node"
    textExprValue n = T.pack "<node:" <> textNodeName (nodeName n) <> ">"

    recordMembers = map (first T.pack)
        [ ( "ifname", RecordSelector $ const ("veth0" :: Text) )
        , ( "ip", RecordSelector $ textIpAddress . nodeIp )
        , ( "network", RecordSelector $ nodeNetwork )
        ]


rootNetworkVar :: TypedVarName Network
rootNetworkVar = TypedVarName (VarName "$ROOT_NET")

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
newSubnet net vname = atomicallyWithIO $ do
    idx <- lift $ nextPrefix (netPrefix net) . map fst <$> readTVar (netSubnets net)
    sub <- newNetwork
        (ipSubnet idx (netPrefix net))
        (netDir net </> maybe (T.unpack $ textNetnsName $ getNetns net) (("sub_"++) . unpackVarName) vname)
    lift $ modifyTVar (netSubnets net) ((idx, sub) :)

    let lan = lanSubnet $ netPrefix sub
        lanIp = IpAddress lan
        bridge = lanIp 1
        router = lanIp 254

    (vethNet, vethSub) <- addVEth (net, "veth_s" <> T.pack (show idx)) (sub, "veth0")
    addAddress vethNet router
    setMaster vethSub (netBridge sub) -- this end needs to go up first, otherwise it
    linkUp    vethSub                 -- sometimes gets stuck with NO-CARRIER for a while.
    linkUp    vethNet

    -- If the new subnet can be split further, routing rule for the whole prefix is needed
    when (allowsSubnets (netPrefix sub)) $ do
        addRoute (netPrefix sub) bridge vethNet router
    addRoute (IpPrefix []) router (netBridge sub) bridge
    return sub { netUpstream = Just vethSub }

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
        <*> pure Nothing
        <*> lift (newTVar [])
        <*> lift (newTVar [])
        <*> pure dir

newNode :: MonadIO m => Network -> VarName -> m Node
newNode nodeNetwork vname = atomicallyWithIO $ do
    let lan = lanSubnet $ netPrefix nodeNetwork
        lanIp = IpAddress lan

    nodes <- lift $ readTVar (netNodes nodeNetwork)
    let nodeName = nextNodeName vname $ map Network.nodeName nodes
        idx = fromIntegral $ 2 + length nodes
        nodeIp = lanIp idx
        nodeDir = netDir nodeNetwork </> ("node_" ++ unpackNodeName nodeName)
    nodeNetns <- addNetworkNamespace $ textNetnsName (getNetns nodeNetwork) <> ":" <> textNodeName nodeName
    (vethNet, nodeUpstream) <- addVEth (nodeNetwork, "veth" <> T.pack (show idx)) (nodeNetns, "veth0")

    postpone $ do
        exists <- doesPathExist nodeDir
        when exists $ ioError $ userError $ nodeDir ++ " exists"
        createDirectoryIfMissing True nodeDir

    let node = Node {..}
    lift $ writeTVar (netNodes nodeNetwork) (node : nodes)

    setMaster vethNet $ netBridge nodeNetwork
    linkUp vethNet
    addAddress nodeUpstream $ nodeIp
    linkUp $ nodeUpstream
    linkUp $ loopback node
    addRoute (IpPrefix []) (lanIp 1) nodeUpstream nodeIp

    return node
