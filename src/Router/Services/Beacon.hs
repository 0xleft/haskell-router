module Router.Services.Beacon (
  start
) where

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.POSIX (getPOSIXTime)
import Router.Layers (BeaconFrame (..), BeaconFrameBody)
import Router.Layers.BeaconFrame (MACHeader (MACHeader), defaultMacHeader, defaultFrameBody, setFrameBodyTimeStamp, setMacHeaderDuration, setMacHeaderSeqControl)

start :: IO ()
start = do
  defaultMac <- defaultMacHeader
  beaconLoop 1024 defaultFrameBody defaultMac 0
  return ()

beaconLoop :: Int -> BeaconFrameBody -> MACHeader -> Int -> IO ()
beaconLoop delay frameBody macHeader seqNum = do
  threadDelay delay

  curTime <- (round . (* 1000000)) <$> getPOSIXTime

  -- TODO: create a beacon frame using the current time
  let curBeaconFrameBody = setFrameBodyTimeStamp curTime
                            $ frameBody
      curMacHeader = setMacHeaderSeqControl seqNum 0
                      $ macHeader
      curBeaconFrame = BeaconFrame {macHeader=curMacHeader,
                                    beaconFrameBody=curBeaconFrameBody}
  

  beaconLoop delay curBeaconFrameBody curMacHeader (succ seqNum)
  return ()


-- createBeacon :: IO String -> IO Integer -> BeaconFrame
-- createBeacon time idkYet =
  -- let macHeader = MACHeader ()