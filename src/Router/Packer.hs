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
import Data.Data (Data, gmapQ, gmapQi, toConstr, constrFields)
import Foreign (Ptr)
import Foreign.Marshal.Array (newArray)
import Data.Bits (shiftR)

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
                                  Nothing -> []
                                )
            Nothing -> []
  False -> getFields x

-- basicaly just a massive catchall case for words and word lists
convertField :: (String, Dynamic) -> [Word8]
convertField (name, d)
  | Just w <- fromDynamic d :: Maybe Word8 = [w]
  | Just w <- fromDynamic d :: Maybe Word16 = [fromIntegral (w `shiftR` 8), fromIntegral w]
  | Just w <- fromDynamic d :: Maybe Word32 = [fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 8), fromIntegral w]
  | Just w <- fromDynamic d :: Maybe Word64 = [fromIntegral (w `shiftR` 56), fromIntegral (w `shiftR` 48), fromIntegral (w `shiftR` 40), fromIntegral (w `shiftR` 32), fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 8), fromIntegral w]
  | Just listW8 <- fromDynamic d :: Maybe [Word8] = listW8
  | Just listW16 <- fromDynamic d :: Maybe [Word16] = concatMap (\w -> convertField ("", toDyn w)) listW16
  | Just listW32 <- fromDynamic d :: Maybe [Word32] = concatMap (\w -> convertField ("", toDyn w)) listW32
  | Just listW64 <- fromDynamic d :: Maybe [Word64] = concatMap (\w -> convertField ("", toDyn w)) listW64

  -- here comes the ugly
  | Just ssid <- fromDynamic d :: Maybe SSID = convertData ssid
  | Just packetType <- fromDynamic d :: Maybe PacketType = convertField $ ("", toDyn $ getPacketType packetType)
  | Just supportedRates <- fromDynamic d :: Maybe SupportedRates = convertData supportedRates
  | Just beaconFrameBody <- fromDynamic d :: Maybe BeaconFrameBody = convertData beaconFrameBody
  | Just macheader <- fromDynamic d :: Maybe MACHeader = convertData macheader

  -- end of ugly
  | Just string <- fromDynamic d :: Maybe String = map (fromIntegral . fromEnum) string
  | otherwise = map (fromIntegral . fromEnum) name

convertFields :: [(String, Dynamic)] -> [Word8]
convertFields fields = concatMap convertField fields

convertData :: Data a => a -> [Word8]
convertData x = convertFields $ getRecursiveFields x

pack :: Packet -> IO (Ptr Word8, Int, [Word8]) -- must free the array afterwards
pack p = do
  let ws = convertData $ topLayer p
  ptr <- newArray ws
  return (ptr, length ws, ws)