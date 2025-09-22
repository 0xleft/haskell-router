module Router.Services.Beacon (
  start
) where

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.POSIX (getPOSIXTime)
import Router.Layers (BeaconFrame (..), BeaconFrameBody)
import Router.Layers.BeaconFrame (MACHeader (..), defaultMacHeader, defaultFrameBody, setFrameBodyTimeStamp, setMacHeaderSeqControl)
import qualified Router.Packer as Packer
import qualified Router.Layers as Layers
import qualified Router.Packet as Packet
import Network.Pcap (openLive, sendPacket, PcapHandle(..))
import Foreign (free)

start :: IO ()
start = do
  interface <- openLive "wlo1" 65535 False 0 -- todo replace the interface to get from command line
  beaconLoop 1024 interface defaultFrameBody defaultMacHeader 0
  return ()

beaconLoop :: Int -> PcapHandle -> BeaconFrameBody -> MACHeader -> Int -> IO ()
beaconLoop delay interface frameBody macHeader seqNum = do
  threadDelay delay

  curTime <- (round . (* 1000000)) <$> getPOSIXTime

  -- TODO: create a beacon frame using the current time
  let curBeaconFrameBody = setFrameBodyTimeStamp curTime
                            $ frameBody
      curMacHeader = setMacHeaderSeqControl seqNum 0
                      $ macHeader
      bF = BeaconFrame curMacHeader frameBody
      beaconFrame = Layers.PacketLinkLayer (Layers.LinkLayerBeaconFrame bF)
      beaconFramePacket = Packet.Packet beaconFrame

  -- (ptr, len, ws) <- Packer.pack beaconFramePacket
  -- sendPacket interface ptr len
-- 
  -- free ptr

  beaconLoop delay interface curBeaconFrameBody curMacHeader (succ seqNum)
  return ()


-- createBeacon :: IO String -> IO Integer -> BeaconFrame
-- createBeacon time idkYet =
  -- let macHeader = MACHeader ()