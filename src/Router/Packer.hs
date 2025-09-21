{-# LANGUAGE DeriveDataTypeable #-}

module Router.Packer (
  hasField,
  getFields,
  getRecursiveFields,
  convertFields,
  pack
) where

import Router.Layers.BeaconFrame (SSID(..), BeaconFrameBody(..), MACHeader(..), SupportedRates(..))
import Router.Layers.Ethernet (PacketType(..), getPacketType)
import Router.Packet (Packet(..))
import qualified Data.ByteString.Char8 as B
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Router.Layers (PacketLayer(..))
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Maybe (listToMaybe)
import Text.Printf (printf)
import Data.Data (Data, gmapQ, gmapQi, toConstr, constrFields)

getFields :: Data a => a -> [(String, Dynamic)]
getFields x =
  let fields = constrFields (toConstr x)
      values = gmapQ toDyn x
  in if not (Prelude.null fields) -- if not empty
    then zip fields values -- then return fields
    else gmapQi 0 getFields x  -- Try the first child, if any

hasField :: Data a => String -> a -> Bool
hasField fieldName x = fieldName `elem` (map fst (getFields x))

getRecursiveFields :: Data a => a -> [(String, Dynamic)]
getRecursiveFields x = case (hasField "parent" x) of
  True -> case listToMaybe (filter (\(x, _) -> x == "parent") (getFields x)) of
            Just (_, parent) -> (case fromDynamic parent :: Maybe PacketLayer of  ---- working around types shitfuckery
                                  Just castValue -> getRecursiveFields castValue ++ (filter (\(x, _) -> x /= "parent") (getFields x))
                                  Nothing -> [("test", toDyn x)]
                                )
            Nothing -> []
  False -> getFields x

-- basicaly just a massive catchall case for words and word lists
convertField :: (String, Dynamic) -> String
convertField (name, d)
  | Just w8 <- fromDynamic d :: Maybe Word8 = printf "%02X" w8
  | Just w16 <- fromDynamic d :: Maybe Word16 = printf "%04X" w16
  | Just w32 <- fromDynamic d :: Maybe Word32 = printf "%08X" w32
  | Just w64 <- fromDynamic d :: Maybe Word64 = printf "%016X" w64
  | Just listW8 <- fromDynamic d :: Maybe [Word8] = concat (map (\x -> printf "%02X" x) listW8)
  | Just listW16 <- fromDynamic d :: Maybe [Word16] = concat (map (\x ->  printf "%04X" x) listW16)
  | Just listW32 <- fromDynamic d :: Maybe [Word32] = concat (map (\x ->  printf "%08X" x) listW32)
  | Just listW64 <- fromDynamic d :: Maybe [Word64] = concat (map (\x ->  printf "%016X" x) listW64)

  -- here comes the ugly
  | Just ssid <- fromDynamic d :: Maybe SSID = convertData ssid
  | Just packetType <- fromDynamic d :: Maybe PacketType = printf "%04X" $ getPacketType packetType
  | Just supportedRates <- fromDynamic d :: Maybe SupportedRates = convertData supportedRates
  | Just beaconFrameBody <- fromDynamic d :: Maybe BeaconFrameBody = convertData beaconFrameBody
  | Just macheader <- fromDynamic d :: Maybe MACHeader = convertData macheader

  -- end of ugly
  | Just int <- fromDynamic d :: Maybe Integer = printf "%X" int -- TODO change this?
  | Just string <- fromDynamic d :: Maybe String = string
  | otherwise = error "You need to implement conversion from the data type you made to String of hexadecimal. Non implemented field: " ++ name

convertFields :: [(String, Dynamic)] -> [String]
convertFields fields = map convertField fields

convertData :: Data a => a -> String
convertData x = concat $ convertFields $ getRecursiveFields x

pack :: Packet -> B.ByteString
pack p = B.pack $ convertData $ topLayer p 