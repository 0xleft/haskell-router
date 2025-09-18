{-# LANGUAGE DuplicateRecordFields #-}

module Router.Layers (
  LinkLayer(..),
  NetworkLayer(..),
  TransferLayer(..),
  ApplicationLayer(..),
  PacketLayer(..)
) where

import Data.Word (Word32, Word16, Word8)

data PacketLayer = PacketLinkLayer LinkLayer | PacketNetworkLayer NetworkLayer | PacketTransferLayer TransferLayer | PacketApplicationLayer ApplicationLayer

data LinkLayer = LinkLayerEth Ethernet | LinkLayerWifi Wifi
data NetworkLayer = NetworkLayerIP IP
data TransferLayer = TransferLayerUDP UDP | TransferLayerTCP TCP
data ApplicationLayer = ApplicationLayerTxt Txt | ApplicationLayerHtml Html

data Ethernet = Ethernet {
  destinationMac :: [Word8], -- of length 6!
  sourceMac :: [Word8], -- also of length 6
  packetType :: Word16 -- TODO figure out how to set it using an enum but still keeping the binary represenation
}

data Wifi = Wifi { -- todo? do we even need this?
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

data TCP = TCP {
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