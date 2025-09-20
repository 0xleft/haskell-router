module Main (main) where

import Router.Packet (Packet(..), PacketQueue(..))
import Router.Services.Beacon as Beacon
import Router.Services.Receiver as Receiver
import Router.Services.Sender as Sender
import Control.Concurrent (forkIO)

-- main thread will be processing one packet at the time from the queue
-- a thread will be listening for packets and adding them to the queue

main :: IO ()
main = do
  let pq = PacketQueue { maxLength = 1000, packets = [] }
  -- todo replace with MVar

  _ <- forkIO $ Beacon.start -- thread
  _ <- forkIO $ Receiver.start pq  -- thread

  _ <- Sender.start pq -- main thread

  return ()