{-# LANGUAGE DeriveDataTypeable #-}

module Router.Packet (
  Packet(..),
  PacketQueue(..),

  hasField,
  getFields,
  getRecursiveFields
) where

import qualified Data.ByteString.Char8 as B

import Data.Data (Data, gmapQ, gmapQi, Typeable, toConstr, constrFields)
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Router.Layers (PacketLayer(..))
import Data.Word (Word32, Word16, Word8)
import Data.Maybe (listToMaybe)

data Packet = Packet {
  topLayer :: PacketLayer
} deriving (Data, Typeable)

class PacketPacker p where
  pack :: p -> B.ByteString

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
                                  Just castValue -> getFields x ++ getRecursiveFields castValue
                                  Nothing -> [("test", toDyn x)]
                                )
            Nothing -> []
  False -> getFields x

instance PacketPacker Packet where
  pack p = case (hasField "parent" (topLayer p)) of
    True -> B.pack ""
    False -> B.pack ""

data PacketQueue = PacketQueue {
  maxLength :: Int,
  packets :: [Packet]
}

class Queue q where
  append :: q -> Packet -> q
  take :: q -> Packet

instance Queue PacketQueue where
  append packetQueue packet = case ((length (packets packetQueue)) <= (maxLength packetQueue)) of
    True -> packetQueue
    False -> packetQueue
  take packetQueue = Packet {  }