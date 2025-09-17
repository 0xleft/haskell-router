module Router.Packet (
  LinkPacket(..),
  IPPacket(..)
) where

import Data.Word (Word32, Word16, Word8)

data LinkPacket = LinkPacket {
  destinationMac :: [Word8], -- of length 6!
  sourceMac :: [Word8], -- also of length 6
  packetType :: Word16 -- TODO figure out how to set it using an enum but still keeping the binary represenation
}

-- https://www.geeksforgeeks.org/computer-networks/tcp-ip-packet-format/ and also the wireshark file
data IPPacket = IPPacket {
  linkPacket :: LinkPacket,
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
  options :: [Word8] -- basicaly whatever is left until we find 0x00 (aka EOL)
}