{-# LANGUAGE DuplicateRecordFields, DeriveDataTypeable #-}

module Router.Layers.BeaconFrame (
  SSID(..),
  MACHeader(..),
  BeaconFrameBody(..),
  SupportedRates(..),
  getSupportedRates,
  getFrameControl,
  getMacAddress,
  setMacHeaderBSsid,
  setMacHeaderDestination,
  setMacHeaderDuration,
  setMacHeaderSeqControl,
  defaultMacHeader,
  defaultFrameBody,
  setFrameBodyBeaconInterval,
  setFrameBodyCapabilityInfo,
  setFrameBodySSID,
  setFrameBodyTimeStamp,
  setFrameBodySupportedRates,
  setMacHeaderSource
) where

import Router.Util (intToWord8)
import Data.Word (Word64,Word16, Word8)
import Data.Data (Data)
import Data.Char (ord)
import Data.Bits
import Router.RouterInfo (macAddress)

-- Source: https://tbhaxor.com/mac-header-format-in-detail/
data MACHeader = MACHeader {
  frameControl :: Word16, -- A lot of information about frame: https://tbhaxor.com/mac-header-format-in-detail/
  duration :: Word16, 
  -- The following addresses can have different meanig depending on To SD and From SD flag in frameControl option: https://mrncciew.com/2014/09/28/cwap-mac-headeraddresses/
  destinationMacAddress :: [Word8], -- Is actually 6 word 8's because it is a mac address
  sourceMacAddress :: [Word8], -- Is actually 6 word 8's because it is a mac address
  bSsidMacAddress :: [Word8], -- bSsid address should be the same as source when using beacon frame
  seqControl :: Word16 -- https://mrncciew.com/2014/11/01/cwap-mac-header-sequence-control/ idk if we need this
  -- macAddress4 :: Word64, -- I dont think it matters for beacon frames
  -- htControl :: Word32, -- Probably won't need it since it is only used for 802.11n and 802.11ac  
} deriving (Data)

-- https://mrncciew.com/2014/10/08/802-11-mgmt-beacon-frame/
-- WireShark capture of BeaconFrame: https://documentation.meraki.com/MR/Wireless_Troubleshooting/Analyzing_Wireless_Packet_Captures
data BeaconFrameBody = BeaconFrameBody { 
  timeStamp :: Word64, -- Time in microseconds since the access point has been active
  beaconInterval :: Word16, -- Time in TU (1 TU = 1024 microseconds) ; default = 100 TU (102.4 milliseconds)
  capabilityInfo :: Word16, -- Advertised capabilities of network
  ssid :: SSID,
  supportedRates :: SupportedRates
} deriving (Data)

-- https://mrncciew.com/2014/10/08/802-11-mgmt-beacon-frame/ (Sec. 4 on SSID)
data SSID = SSID {
  elId :: Word8, -- I think in all cases it should be 0b00
  tagLength :: Word8, -- Just the length if the string
  ssidName :: String -- A string with length of tagLength 
} deriving (Data)

getSSID :: String -> SSID
getSSID "" = SSID (0) (0) ("") 
getSSID string =
  let str_len = (length string)
  in if (str_len > 32)
    then error "SSID name is too long"
    else SSID (0) (intToWord8 str_len) string 

data SupportedRates = SupportedRates {
  elId :: Word8,
  length_array :: Word8, -- number of supported rates
  allSupportedRates :: [Word8] -- Array of size(length) that contains all the supported rates
} deriving (Data)

defaultSupportedRates :: SupportedRates
defaultSupportedRates = SupportedRates 0 0 [] -- TODO: MAKE THESE VALUES MAKE SENSE

-- Rates is in Int * 500Kbps, thus 12 = (12 * 500kbps) = 6 Mbps
-- 7th bit is flipped on by default to set it to basic rate, intead of supported rate
getSupportedRates :: [Int] -> SupportedRates
getSupportedRates rates = 
  SupportedRates  (0)
                  (intToWord8 (length rates)) 
                  (map (\r -> (intToWord8 r) .|. 128) rates)

setFrameBodyTimeStamp :: Int -> BeaconFrameBody -> BeaconFrameBody
setFrameBodyTimeStamp time body = body {timeStamp = (fromIntegral time) :: Word64}

setFrameBodyBeaconInterval :: Int -> BeaconFrameBody -> BeaconFrameBody
setFrameBodyBeaconInterval time body = body {beaconInterval = (fromIntegral time) :: Word16}

setFrameBodyCapabilityInfo :: Word16 -> BeaconFrameBody -> BeaconFrameBody
setFrameBodyCapabilityInfo cap body = body {capabilityInfo = cap}

setFrameBodySSID :: String -> BeaconFrameBody -> BeaconFrameBody
setFrameBodySSID name body = body {ssid = (getSSID name)}

setFrameBodySupportedRates :: [Int] -> BeaconFrameBody-> BeaconFrameBody
setFrameBodySupportedRates xs body = body {supportedRates = getSupportedRates xs}

defaultFrameBody :: BeaconFrameBody
defaultFrameBody = 
  BeaconFrameBody  {timeStamp = 0,
                    beaconInterval = 100,
                    capabilityInfo = 0x1511,
                    ssid = getSSID "Awesome Network",
                    supportedRates = defaultSupportedRates}

-- TODO: Make an actual buider for the FramControl
getFrameControl :: Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word16
getFrameControl beaconSubtype managementFrame version toDS fromDS fragment retry powerSaving moreData encrypted order =
  let b x = if x then 1 else 0 :: Word16
      fc =     (fromIntegral beaconSubtype)  `shiftL` 12  
           .|. (fromIntegral managementFrame)  `shiftL` 10  
           .|. (fromIntegral version)  `shiftL` 8  
           .|. b toDS         `shiftL` 7
           .|. b fromDS       `shiftL` 6
           .|. b fragment     `shiftL` 5
           .|. b retry        `shiftL` 4
           .|. b powerSaving  `shiftL` 3
           .|. b moreData     `shiftL` 2
           .|. b encrypted    `shiftL` 1
           .|. b order        `shiftL` 0
  in fc

charToHex :: Char -> Int
charToHex c
  | nOrd >= 48 && nOrd <= 57 = nOrd - 48 -- 1 through 9
  | nOrd >= 97 && nOrd <= 102 = nOrd - 97 + 10  -- a through f
  | otherwise = error "Only works from 1-9 and a-f"
  where nOrd = ord c

hexStringToHex :: String -> Word8
hexStringToHex [x,y]
  = let hex1 = (fromIntegral (charToHex x)) :: Word8
        hex2 = (fromIntegral (charToHex y)) :: Word8
        hexTot = (hex1 `shift` 4) .|. hex2
    in hexTot
hexStringToHex _ = error "Please enter a string of length 2"

defaultMacHeader :: MACHeader
defaultMacHeader = 
  MACHeader {sourceMacAddress = getMacAddress macAddress,
                destinationMacAddress = getMacAddress "ff:ff:ff:ff:ff:ff",
                bSsidMacAddress = getMacAddress macAddress,
                duration = 0,
                seqControl = 0,
                frameControl = 0x8000}

-- Format should be "ff:ff:ff:ff:ff:ff" and returns binary repr
getMacAddress :: String -> [Word8]
getMacAddress [] = []
getMacAddress (x:y:xs) = getMacAddressHelper xs [hexStringToHex ([x,y])]
getMacAddress _ = error "bad pattern"

getMacAddressHelper :: String -> [Word8] -> [Word8]
getMacAddressHelper [] n = n 
getMacAddressHelper (x:y:z:xs) n  -- x = ':' y = 'f' z = 'f'
  = getMacAddressHelper xs (n ++ [hexStringToHex[y,z]])
getMacAddressHelper _ _= error "bad mac address pattern"
  
setMacHeaderDestination :: String -> MACHeader -> MACHeader
setMacHeaderDestination s mac = mac {destinationMacAddress = getMacAddress s}

setMacHeaderSource :: String -> MACHeader -> MACHeader
setMacHeaderSource s mac = mac {sourceMacAddress = getMacAddress s}

setMacHeaderBSsid :: String -> MACHeader -> MACHeader
setMacHeaderBSsid s mac = mac {bSsidMacAddress = getMacAddress s}

setMacHeaderDuration :: Int -> MACHeader -> MACHeader
setMacHeaderDuration dur mac = mac {duration = (fromIntegral dur) :: Word16}

setMacHeaderSeqControl :: Int -> Int -> MACHeader -> MACHeader
setMacHeaderSeqControl seqNum fragNum mac
  = let seqCtrl = ((fromIntegral seqNum) ::Word16) `shift` 4
                  .|. ((fromIntegral fragNum) ::Word16)
    in mac {seqControl = seqCtrl}

