{-# LANGUAGE DeriveDataTypeable #-}

module Router.Packet (
  Packet(..),
  PacketQueue(..),

  hasField
) where

import qualified Data.ByteString.Char8 as B

import Data.Data
import Router.Layers (PacketLayer(..))
import Data.Word (Word32, Word16, Word8)

data Packet = Packet {
  topLayer :: PacketLayer
} deriving (Data, Typeable)

class PacketPacker p where
  pack :: p -> B.ByteString


getFields :: Data a => a -> [String]
getFields x =
  let fields = constrFields (toConstr x)
  in if not (Prelude.null fields) -- if not empty
    then fields -- then return fields
    else gmapQi 0 getFields x  -- Try the first child, if any

hasField :: Data a => String -> a -> Bool
hasField fieldName x = fieldName `elem` (getFields x)

instance PacketPacker Packet where
  pack p = case (hasField "parent" (topLayer p)) of
    True -> B.pack ""
    False -> B.pack ""

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