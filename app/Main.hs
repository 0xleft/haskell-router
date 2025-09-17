module Main (main) where

import Router.Packet (LinkPacket(..), IPPacket(..))

import qualified Data.ByteString.Char8 as B
import Network.Pcap

main :: IO ()
main = do
  -- open device
  dev <- openLive "lo" 65535 False 0
  setFilter dev "ether proto 0xFFFF" True 0

  -- send
  sendPacketBS dev (B.pack "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255Hello, World!")

  -- receive
  loopBS dev (-1) (\_ packet -> putStrLn (show packet))
  return ()