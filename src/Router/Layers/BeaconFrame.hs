{-# LANGUAGE DuplicateRecordFields #-}

module Router.Layers.BeaconFrame (
  SSID(..),
  MACHeader(..),
  BeaconFrameBody(..)
) where

import Router.Util (intToWord8)
import Data.Word (Word64,Word16, Word8)




-- Source: https://tbhaxor.com/mac-header-format-in-detail/
data MACHeader = MACHeader {
  frameControl :: Word16, -- A lot of information about frame: https://tbhaxor.com/mac-header-format-in-detail/
  duration :: Word16, 

  -- The following addresses can have different meanig depending on To SD and From SD flag in frameControl option: https://mrncciew.com/2014/09/28/cwap-mac-headeraddresses/
  sourceMacAddress :: Word64, -- Is actually 6 word 8's because it is a mac address
  destinationMacAddress :: Word64, -- Is actually 6 word 8's because it is a mac address
  bSsidMacAddress :: Word64, -- bSsid address should be the same as source when using beacon frame

  squenceControl :: Word16, -- https://mrncciew.com/2014/11/01/cwap-mac-header-sequence-control/ idk if we need this
  
  -- macAddress4 :: Word64, -- I dont think it matters for beacon frames

  qosControl :: Word16 -- I have no idea what this does

  -- htControl :: Word32, -- Probably won't need it since it is only used for 802.11n and 802.11ac  
}


-- https://mrncciew.com/2014/10/08/802-11-mgmt-beacon-frame/
-- WireShark capture of BeaconFrame: https://documentation.meraki.com/MR/Wireless_Troubleshooting/Analyzing_Wireless_Packet_Captures
data BeaconFrameBody = BeaconFrameBody { 
  timeStamp :: Word64, -- Time in microseconds since the access point has been active
  beaconInterval :: Word16, -- Time in TU (1 TU = 1024 microseconds) ; default = 100 TU (102.4 milliseconds)
  capableInformation :: Word16, -- Advertised capabilities of network
  ssid :: SSID
}

-- https://mrncciew.com/2014/10/08/802-11-mgmt-beacon-frame/ (Sec. 4 on SSID)
data SSID = SSID {
  elId :: Word8, -- I think in all cases it should be 0b00
  tagLength :: Word8, -- Just the length if the string
  ssidName :: String, -- A string with length of tagLength 
  supportedRates :: SupportedRates --
}


getSSID :: String -> SSID
getSSID "" = SSID (0) (0) ("\0") getDefaultSupportedRates
getSSID string =
  let str_len = (length string)
  in if (str_len > 32)
    then error "SSID name is too long"
    else SSID (0) (intToWord8 str_len) string getDefaultSupportedRates


data SupportedRates = SupportedRates {
  elId :: Word8,
  length_array :: Word8, -- number of supported rates
  allSupportedRates :: [Word8] -- Array of size(length) that contains all the supported rates
}



getDefaultSupportedRates :: SupportedRates
getDefaultSupportedRates = SupportedRates 0 0 [] -- TODO: MAKE THESE VALUES MAKE SENSE