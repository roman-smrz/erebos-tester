module Network.Ip (
    IpPrefix(..),
    textIpNetwork,

    IpAddress(..),
    textIpAddress,
    textIpAddressCidr,

    allowsSubnets,
    ipSubnet,
    lanSubnet,

    MonadPIO(..),

    NetworkNamespace,
    HasNetns(..),
    addNetworkNamespace,
    textNetnsName,
    callOn,

    Link(..),
    Loopback, loopback,
    VEth, addVEth,
    Bridge, addBridge,
    addAddress,
    setMaster,
    linkUp, linkDown,
) where

import Control.Monad.Writer

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word

import System.Process

newtype IpPrefix = IpPrefix [Word8]
    deriving (Eq, Ord)

textIpNetwork :: IpPrefix -> Text
textIpNetwork (IpPrefix prefix) =
    T.intercalate "." (map (T.pack . show) $ prefix ++ replicate (4 - length prefix) 0)
    <> "/" <> T.pack (show (8 * length prefix))

data IpAddress = IpAddress IpPrefix Word8
    deriving (Eq, Ord)

textIpAddress :: IpAddress -> Text
textIpAddress (IpAddress (IpPrefix prefix) num) =
    T.intercalate "." $ map (T.pack . show) $ prefix ++ replicate (3 - length prefix) 0 ++ [num]

textIpAddressCidr :: IpAddress -> Text
textIpAddressCidr ip@(IpAddress (IpPrefix prefix) _) =
    textIpAddress ip <> "/" <> T.pack (show (8 * length prefix))

allowsSubnets :: IpPrefix -> Bool
allowsSubnets (IpPrefix prefix) = length prefix < 3

ipSubnet :: Word8 -> IpPrefix -> IpPrefix
ipSubnet num (IpPrefix prefix) = IpPrefix (prefix ++ [num])

lanSubnet :: IpPrefix -> IpPrefix
lanSubnet (IpPrefix prefix) = IpPrefix (take 3 $ prefix ++ repeat 0)


class Monad m => MonadPIO m where
    postpone :: IO () -> m ()

instance MonadPIO IO where
    postpone = id

instance Monad m => MonadPIO (WriterT [IO ()] m) where
    postpone = tell . (:[])


newtype NetworkNamespace = NetworkNamespace
    { netnsName :: Text
    }
    deriving (Eq, Ord)

class HasNetns a where getNetns :: a -> NetworkNamespace
instance HasNetns NetworkNamespace where getNetns = id

addNetworkNamespace :: MonadPIO m => Text -> m NetworkNamespace
addNetworkNamespace name = do
    postpone $ callCommand $ T.unpack $ "ip netns add \"" <> name <> "\""
    return $ NetworkNamespace
        { netnsName = name
        }

textNetnsName :: NetworkNamespace -> Text
textNetnsName = netnsName

callOn :: HasNetns a => a -> Text -> IO ()
callOn n cmd = callCommand $ T.unpack $ "ip netns exec \"" <> ns <> "\" " <> cmd
    where NetworkNamespace ns = getNetns n


data Link a = Link
    { linkName :: Text
    , linkNetns :: NetworkNamespace
    }

instance HasNetns (Link a) where getNetns = linkNetns

data Loopback

loopback :: HasNetns n => n -> Link Loopback
loopback = Link "lo" . getNetns

data VEth

addVEth :: (HasNetns n, HasNetns n', MonadPIO m) => (n, Text) -> (n', Text) -> m (Link VEth, Link VEth)
addVEth (netns, name) (netns', name') = do
    postpone $ callOn netns $ "ip link add \"" <> name <> "\" type veth peer name \"" <> name' <> "\" netns \"" <> textNetnsName (getNetns netns') <> "\""
    return $ (,)
        (Link name  $ getNetns netns )
        (Link name' $ getNetns netns')

data Bridge

addBridge :: (HasNetns n, MonadPIO m) => n -> Text -> m (Link Bridge)
addBridge netns name = do
    postpone $ callOn netns $ "ip link add name \"" <> name <> "\" type bridge"
    return $ Link name $ getNetns netns

addAddress :: MonadPIO m => Link a -> IpAddress -> m ()
addAddress link addr@(IpAddress prefix _) = do
    let bcast = IpAddress prefix 255
    postpone $ callOn link $ "ip addr add " <> textIpAddressCidr addr <> " broadcast " <> textIpAddress bcast <> " dev \"" <> linkName link <> "\""

setMaster :: MonadPIO m => Link a -> Link Bridge -> m ()
setMaster link bridge = postpone $ do
    when (getNetns link /= getNetns bridge) $ fail "link and bridge in different network namespaces"
    callOn link $ "ip link set dev \"" <> linkName link <> "\" master \"" <> linkName bridge <> "\""

linkUp :: MonadPIO m => Link a -> m ()
linkUp link = do
    postpone $ callOn link $ "ip link set dev \"" <> linkName link <> "\" up"

linkDown :: MonadPIO m => Link a -> m ()
linkDown link = do
    postpone $ callOn link $ "ip link set dev \"" <> linkName link <> "\" down"
