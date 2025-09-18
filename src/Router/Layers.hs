{-# LANGUAGE DuplicateRecordFields #-}

module Router.Layers (
  LinkLayer(..),
  NetworkLayer(..),
  TransferLayer(..),
  ApplicationLayer(..),
  PacketLayer(..),

) where

import Data.Word (Word32, Word16, Word8)
import Router.Layers.Ethernet(PacketType(..))



-- generic catch all case for layers
data PacketLayer = PacketLinkLayer LinkLayer | PacketNetworkLayer NetworkLayer | PacketTransferLayer TransferLayer | PacketApplicationLayer ApplicationLayer

data LinkLayer = LinkLayerEth Ethernet | LinkLayerWifi Wifi
data NetworkLayer = NetworkLayerIP IP
data TransferLayer = TransferLayerUDP UDP | TransferLayerTCP TCP
data ApplicationLayer = ApplicationLayerTxt Txt | ApplicationLayerHtml Html

data Ethernet = Ethernet {
  destinationMac :: [Word8], -- of length 6!
  sourceMac :: [Word8], -- also of length 6
  packetType :: PacketType
}

data Wifi = Wifi { -- todo? do we even need this?; No, but otherwise the LinkLayer type looked really empty, we can remove it now that the structure is defined
}

-- https://www.geeksforgeeks.org/computer-networks/tcp-ip-packet-format/ and also the wireshark file
data IP = IP {
  versionAndHeaderLenth :: Word8, -- ugly but we dont have Word4 type :(
  typeOfService :: Word8,
  totalLength :: Word16,
  identification :: Word16,
  flagsAndFragmentOffset :: Word16, -- otherwise it would be 3 bits for flag and 13 for fragment offset but thats a no no :( 
  timeToLive :: Word8,
  protocol :: Word8,
  headerChecksum :: Word16,
  sourceIPAddr :: Word32,
  destinationIPAddr :: Word32,
  options :: [Word8], -- basicaly whatever is left until we find 0x00 (aka EOL)
  parent :: LinkLayer
}

-- Source: https://datatracker.ietf.org/doc/html/rfc9293
data TCP = TCP {
  sourcePort :: Word16,
  destinationPort :: Word16,
  squenceNumber :: Word32,
  acknowlegdementNumber :: Word32,
  dOffset :: Word8, -- Actually 4 bit, number of 32 Bit words in the header, Thus indicating where the data begins
  rSrvd :: Word8, -- Actually 4 bit, basically must be 0 if both end-points don't implement it
  controlBits :: Word8, -- Look at documentation what every bit is set for.
  window :: Word32, -- Normally it is 16 Word, but RFC recommends it is 32 bit. It is the number of octets we can accept
  checkSum :: Word16, -- TODO: I can't be bothered to figure this out rn
  urgentPointer :: Word16, -- Only used when URG bit in controlBits is set, points to the the first data that is not urgent.
  options :: Integer, -- Size of (dOffset-5)
  parent :: NetworkLayer
}

data UDP = UDP {
  parent :: NetworkLayer
}

data Txt = Txt {
  parent :: TransferLayer
}


data Html = Html {
  parent :: TransferLayer
}

data Packet = Packet {
  
}