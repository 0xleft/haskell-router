{-# LANGUAGE DeriveDataTypeable #-}

module Router.Layers.Ethernet (
  PacketType(..)
) where 

import Router.Types (Packable(..))
import Data.Word (Word16)
import Data.Data
import qualified Data.ByteString.Char8 as B

data PacketType = IPv6Packet | IPv4Packet deriving (Data)
-- Gives the correct Word16 type for the data packet
getPacketType :: PacketType -> Word16
getPacketType IPv6Packet = 35037 -- IPv6 is bit value 88DD Which is 35037 in decimal
getPacketType IPv4Packet = 2048 -- IPv4 is bit value 0800 which is 2048 in decimal

instance Packable PacketType where
  pack IPv6Packet = B.pack ""
  pack IPv4Packet = B.pack ""