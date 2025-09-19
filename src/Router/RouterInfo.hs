module Router.RouterInfo (
  macAddress
) where

import System.IO

networkDevice :: String
networkDevice = "lo"

macAddress :: IO String
macAddress = do 
  contents <- readFile ("/sys/class/net/" ++ networkDevice ++ "/address")
  return contents

  