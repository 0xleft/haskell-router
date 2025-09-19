module Router.Services.Sender (
  start
) where
  
import Router.Packet (PacketQueue)

start :: PacketQueue -> IO ()
start pq = do
  return ()