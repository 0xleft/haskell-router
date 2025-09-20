{-# LANGUAGE DuplicateRecordFields, DeriveDataTypeable #-}

module Router.Layers (
  PacketLayer(..),

  LinkLayer(..),
  NetworkLayer(..),
  TransferLayer(..),
  ApplicationLayer(..),

  BeaconFrame(..),
  BeaconFrameBody(..),
  Ethernet(..),
  Ip(..),
  Tcp(..),
  Udp(..),
  Txt(..),
  Html(..)
) where

import Data.Word (Word64, Word32, Word16, Word8)
import Router.Layers.BeaconFrame (SSID(..), MACHeader(..), BeaconFrameBody(..))
import Router.Layers.Ethernet (PacketType(..))
import Data.Data

-- generic catch all case for layers
data PacketLayer = PacketLinkLayer LinkLayer | PacketNetworkLayer NetworkLayer | PacketTransferLayer TransferLayer | PacketApplicationLayer ApplicationLayer deriving (Data)

data LinkLayer = LinkLayerEth Ethernet | LinkLayerBeaconFrame BeaconFrame deriving (Data)
data NetworkLayer = NetworkLayerIp Ip deriving (Data)
data TransferLayer = TransferLayerUdp Udp | TransferLayerTcp Tcp deriving (Data)
data ApplicationLayer = ApplicationLayerTxt Txt | ApplicationLayerHtml Html deriving (Data)

data BeaconFrame = BeaconFrame {
  macHeader :: MACHeader,
  beaconFrameBody :: BeaconFrameBody
} deriving (Data)

data Ethernet = Ethernet {
  destinationMac :: [Word8], -- of length 6!
  sourceMac :: [Word8], -- also of length 6
  packetType :: PacketType
} deriving (Data)

-- https://www.geeksforgeeks.org/computer-networks/tcp-ip-packet-format/ and also the wireshark file
data Ip = Ip {
  versionAndHeaderLength :: Word8, -- ugly but we dont have Word4 type :(
  typeOfService :: Word8,
  totalLength :: Word16,
  identification :: Word16,
  flagsAndFragmentOffset :: Word16, -- otherwise it would be 3 bits for flag and 13 for fragment offset but thats a no no :( 
  timeToLive :: Word8,
  protocol :: Word8,
  headerChecksum :: Word16,
  sourceIpAddr :: Word32,
  destinationIpAddr :: Word32,
  options :: [Word8], -- basicaly whatever is left until we find 0x00 (aka EOL)
  parent :: PacketLayer
} deriving (Data)

-- Source: https://datatracker.ietf.org/doc/html/rfc9293
data Tcp = Tcp {
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
  parent :: PacketLayer
} deriving (Data)

data Udp = Udp {
  parent :: PacketLayer
} deriving (Data)

data Txt = Txt {
  parent :: PacketLayer
} deriving (Data)

data Html = Html {
  parent :: PacketLayer
} deriving (Data)