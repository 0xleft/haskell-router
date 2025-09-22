module Router.RouterInfo (
  macAddress
) where

networkDevice :: String
networkDevice = "lo"

macAddress :: String
macAddress = "00:00:00:00:00:00"
-- macAddress :: IO String
-- macAddress = do 
  -- contents <- readFile ("/sys/class/net/" ++ networkDevice ++ "/address")
  -- return (reverse (drop 1 (reverse contents))) -- Drops the last '\n' in the string