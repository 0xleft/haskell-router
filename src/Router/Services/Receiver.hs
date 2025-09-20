module Router.Services.Receiver (
  start
) where

import Router.Packet (PacketQueue)

start :: PacketQueue -> IO ()
start pq = do
  return ()