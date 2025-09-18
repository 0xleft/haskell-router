module Router.Packet (
  Packet(..),
  PacketQueue(..)
) where

import Router.Layers (PacketLayer(..))
import Data.Word (Word32, Word16, Word8)

data Packet = Packet {
  topLayer :: PacketLayer
}

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