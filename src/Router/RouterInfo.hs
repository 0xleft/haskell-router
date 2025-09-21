module Router.RouterInfo (
  macAddress
) where

networkDevice :: String
networkDevice = "lo"

macAddress :: IO String
macAddress = do 
  contents <- readFile ("/sys/class/net/" ++ networkDevice ++ "/address")
  return contents