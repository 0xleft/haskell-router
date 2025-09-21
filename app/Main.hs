module Main (main) where

import Router.Packet (PacketQueue(..))
import Router.Services.Beacon as Beacon ( start )
import Router.Services.Receiver as Receiver ( start )
import Router.Services.Sender as Sender ( start )
import Control.Concurrent (forkIO)
import Network.Pcap (openLive, sendPacket)
import qualified Router.Packer as Packer
import qualified Router.Packet as Packet
import qualified Router.Layers as Layers
import qualified Router.Layers.Ethernet as Ethernet
import Foreign (free)

-- main thread will be processing one packet at the time from the queue
-- a thread will be listening for packets and adding them to the queue

main :: IO ()
main = do
  let pq = PacketQueue { maxLength = 1000, packets = [] } -- todo replace with MVar

      eth = Layers.Ethernet [0] [0] Ethernet.IPv4Packet
      ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
      ip = Layers.Ip 0 0 0 0 0 0 0 0 0 0 [] ethernetLayer
      ipLayer = Layers.PacketNetworkLayer (Layers.NetworkLayerIp ip)
      tcp = Layers.Tcp 0 0 0 0 0 0 0 0 0 0 0 ipLayer
      tcpLayer = Layers.PacketTransferLayer (Layers.TransferLayerTcp tcp)
      packet = Packet.Packet tcpLayer

  (ptr, len, ws) <- Packer.pack packet

  putStrLn $ show ws

  interface <- openLive "wlo1" 65535 False 0
  sendPacket interface ptr len

  free ptr

  _ <- forkIO $ Beacon.start -- thread
  _ <- forkIO $ Receiver.start pq  -- thread

  _ <- Sender.start pq -- main thread

  return ()