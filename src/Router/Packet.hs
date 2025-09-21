{-# LANGUAGE DeriveDataTypeable #-}

module Router.Packet (
  Packet(..),
  PacketQueue(..),
) where

import Data.Data (Data, Typeable)
import Router.Layers (PacketLayer(..))

data Packet = Packet {
  topLayer :: PacketLayer
} deriving (Data, Typeable)

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