module Router.Layers.Ethernet (
  PacketType(..),
) where 

import Data.Word (Word32, Word16, Word8)

data PacketType = IPv6Packet | IPv4Packet
-- Gives the correct Word16 type for the data packet
getPacketType :: PacketType -> Word16
getPacketType IPv6Packet = 35037 -- IPv6 is bit value 88DD Which is 35037 in decimal
getPacketType IPv4Packet = 2048 -- IPv4 is bit value 0800 which is 2048 in decimal