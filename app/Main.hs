module Main (main) where

import Router.Packet (LinkPacket(..), IPPacket(..))

import qualified Data.ByteString.Char8 as B
import Network.Pcap


-- main thread will be processing one packet at the time from the queue
-- a thread will be listening for packets and adding them to the queue

main :: IO ()
main = do
  -- open device
  interface <- openLive "lo" 65535 False 0

  -- send
  sendPacketBS interface (B.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255Hello, World!")

  -- receive
  loopBS interface (-1) (\header body -> putStrLn (show body))
  return ()