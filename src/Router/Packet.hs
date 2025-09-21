{-# LANGUAGE DeriveDataTypeable #-}

module Router.Packet (
  Packet(..),
  PacketQueue(..),

  hasField,
  getFields,
  getRecursiveFields
) where

import Data.Data (Data, gmapQ, gmapQi, Typeable, toConstr, constrFields)
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Router.Layers (PacketLayer(..))
import Data.Maybe (listToMaybe)

data Packet = Packet {
  topLayer :: PacketLayer
} deriving (Data, Typeable)

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