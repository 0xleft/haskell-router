module Router.Packet (
  LinkPacket(..),
  IPPacket(..),
  PacketType(..),
  getPacketType
  Packet(..),
  PacketQueue(..)
) where

import Router.Layers (PacketLayer(..))
import Data.Word (Word32, Word16, Word8)

data Packet = Packet {
  topLayer :: PacketLayer
}

data PacketType = IPv6Packet | IPv4Packet
-- Gives the correct Word16 type for the data packet
getPacketType :: PacketType -> Word16
getPacketType IPv6Packet = 35037 -- IPv6 is bit value 88DD Which is 35037 in decimal
getPacketType IPv4Packet = 2048 -- IPv4 is bit value 0800 which is 2048 in decimal

data PacketQueue = PacketQueue {
  maxLength :: Int,
  packets :: [Packet]
}

class Queue q where
  append :: q -> Packet -> q
  take :: q -> Packet

instance Queue PacketQueue where
  append packetQueue packet = case ((length (packets packetQueue)) <= (maxLength packetQueue)) of
    True -> packetQueue
    False -> packetQueue
  take packetQueue = Packet {  }
