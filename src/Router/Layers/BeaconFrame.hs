
module Router.Layers.BeaconFrame (

) where

import Router.Layers (SSID(..))
import Router.Util (intToWord8)


getSSID :: String -> SSID
getSSID "" = SSID (0) (0) ("\0")
getSSID string =
  let str_len = (length string)
  in if (str_len > 32)
    then error "SSID name is too long"
    else SSID (0) (intToWord8 str_len) string