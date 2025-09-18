
module Router.Layers.BeaconFrame (
  SSID(..)
) where

import Router.Util (intToWord8)
import Data.Word (Word8)

getSSID :: String -> SSID
getSSID "" = SSID (0) (0) ("\0")
getSSID string =
  let str_len = (length string)
  in if (str_len > 32)
    then error "SSID name is too long"
    else SSID (0) (intToWord8 str_len) string

-- https://mrncciew.com/2014/10/08/802-11-mgmt-beacon-frame/ (Sec. 4 on SSID)
data SSID = SSID {
  elId :: Word8, -- I think in all cases it should be 0b00
  tagLength :: Word8, -- Just the length if the string
  ssidName :: String -- A string with length of tagLength 
}