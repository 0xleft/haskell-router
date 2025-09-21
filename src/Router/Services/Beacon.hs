module Router.Services.Beacon (
  start
) where

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.POSIX (getPOSIXTime)
import Router.Layers (BeaconFrame)

start :: IO ()
start = 
  do
  beaconLoop 1024
  return ()

beaconLoop :: Int -> IO ()
beaconLoop delay = do
  threadDelay delay

  -- TODO: create a beacon frame using the current time
  let curTime = (round . (* 1000000)) <$> getPOSIXTime

  beaconLoop delay 
  return ()


-- createBeacon :: IO String -> IO Integer -> BeaconFrame
-- createBeacon time idkYet =
  -- let macHeader = MACHeader ()