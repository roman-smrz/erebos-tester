module Network.Ip (
    IpPrefix(..),
    textIpNetwork,

    IpAddress(..),
    textIpAddress,
    textIpAddressCidr,

    allowsSubnets,
    ipSubnet,
    lanSubnet,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word

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

