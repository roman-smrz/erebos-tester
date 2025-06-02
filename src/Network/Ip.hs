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
    MonadSTM(..),
    atomicallyWithIO,

    NetworkNamespace,
    HasNetns(..),
    addNetworkNamespace,
    setNetworkNamespace,
    textNetnsName,
    runInNetworkNamespace,
    callOn,

    Link(..),
    Loopback, loopback,
    VEth, addVEth,
    Bridge, addBridge,
    addAddress,
    setMaster,
    linkUp, linkDown,

    Route(..),
    addRoute,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Writer

import Data.Function
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import Data.Word

import Foreign.C.Error
import Foreign.C.Types

import System.Posix.IO
import System.Posix.Types
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


class Monad m => MonadSTM m where
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id

instance MonadSTM m => MonadSTM (WriterT [IO ()] m) where
    liftSTM = lift . liftSTM


atomicallyWithIO :: MonadIO m => WriterT [IO ()] STM a -> m a
atomicallyWithIO act = liftIO $ do
    (x, fin) <- atomically $ runWriterT act
    sequence_ fin
    return x


data NetworkNamespace = NetworkNamespace
    { netnsName :: Text
    , netnsRoutesConfigured :: TVar [Route]
    , netnsRoutesActive :: TVar [Route]
    }

instance Eq NetworkNamespace where
    (==) = (==) `on` netnsName

instance Ord NetworkNamespace where
    compare = compare `on` netnsName

class HasNetns a where getNetns :: a -> NetworkNamespace
instance HasNetns NetworkNamespace where getNetns = id

addNetworkNamespace :: (MonadPIO m, MonadSTM m) => Text -> m NetworkNamespace
addNetworkNamespace netnsName = do
    postpone $ callCommand $ T.unpack $ "ip netns add \"" <> netnsName <> "\""
    netnsRoutesConfigured <- liftSTM $ newTVar []
    netnsRoutesActive <- liftSTM $ newTVar []
    return $ NetworkNamespace {..}

setNetworkNamespace :: MonadIO m => NetworkNamespace -> m ()
setNetworkNamespace netns = liftIO $ do
    let path = "/var/run/netns/" <> T.unpack (textNetnsName netns)
        open = openFd path ReadOnly defaultFileFlags { cloexec = True }
    res <- bracket open closeFd $ \(Fd fd) -> do
        c_setns fd c_CLONE_NEWNET
    when (res /= 0) $ do
        throwErrno "setns failed"

foreign import ccall unsafe "sched.h setns" c_setns :: CInt -> CInt -> IO CInt
c_CLONE_NEWNET :: CInt
c_CLONE_NEWNET = 0x40000000

runInNetworkNamespace :: NetworkNamespace -> IO a -> IO a
runInNetworkNamespace netns act = do
    mvar <- newEmptyMVar
    void $ forkOS $ do
        setNetworkNamespace netns
        putMVar mvar =<< act
    takeMVar mvar


textNetnsName :: NetworkNamespace -> Text
textNetnsName = netnsName

callOn :: HasNetns a => a -> Text -> IO ()
callOn n cmd = runInNetworkNamespace (getNetns n) $ callCommand $ T.unpack cmd


data Link a = Link
    { linkName :: Text
    , linkNetns :: NetworkNamespace
    }
    deriving (Eq)

data SomeLink = forall a. Typeable a => SomeLink (Link a)

instance Eq SomeLink where
    SomeLink a == SomeLink b
        | Just b' <- cast b = a == b'
        | otherwise = False

liftSomeLink :: (forall a. Link a -> b) -> SomeLink -> b
liftSomeLink f (SomeLink x) = f x

instance HasNetns (Link a) where getNetns = linkNetns
instance HasNetns SomeLink where getNetns = liftSomeLink linkNetns

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

linkUp :: (Typeable a, MonadPIO m, MonadSTM m) => Link a -> m ()
linkUp link = do
    routes <- liftSTM $ filter ((== SomeLink link) . routeDev) <$> readTVar (netnsRoutesConfigured (getNetns link))
    liftSTM $ modifyTVar (netnsRoutesActive (getNetns link)) $ (routes ++)
    postpone $ do
        callOn link $ "ip link set dev \"" <> linkName link <> "\" up"
        -- add back routes that were automatically removed by kernel when the link went down
        mapM_ applyRoute routes

linkDown :: (Typeable a, MonadPIO m, MonadSTM m) => Link a -> m ()
linkDown link = do
    -- routes using this device will be automatically removed by kernel
    liftSTM $ modifyTVar (netnsRoutesActive (getNetns link)) $ filter ((/= SomeLink link) . routeDev)
    postpone $ callOn link $ "ip link set dev \"" <> linkName link <> "\" down"


data Route = Route
    { routePrefix :: IpPrefix
    , routeVia :: IpAddress
    , routeDev :: SomeLink
    , routeSrc :: IpAddress
    }

addRoute :: Typeable a => IpPrefix -> IpAddress -> Link a -> IpAddress -> WriterT [IO ()] STM ()
addRoute routePrefix routeVia link routeSrc = do
    let routeDev = SomeLink link
        route = Route {..}
    lift $ do
        modifyTVar (netnsRoutesConfigured (getNetns link)) (route:)
        modifyTVar (netnsRoutesActive (getNetns link)) (route:)
    postpone $ applyRoute route

applyRoute :: Route -> IO ()
applyRoute route = callOn (routeDev route) $ "ip route add "
    <> textIpNetwork (routePrefix route)
    <> " via " <> textIpAddress (routeVia route)
    <> " dev " <> linkName `liftSomeLink` (routeDev route)
    <> " src " <> textIpAddress (routeSrc route)
