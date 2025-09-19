module Router.Services.Receiver (
  start
) where

start :: PacketQueue ->  IO ()
start = do
  return ()