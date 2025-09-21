module Router.Util (
  intToWord8,
) where

import Data.Word (Word64, Word32,Word16,Word8)

intToWord8 :: Int -> Word8
intToWord8 n
  | n < 0   = error "Value too small for Word8"
  | n > 255 = error "value too big for Word8"
  | otherwise = (fromIntegral n)

intToWord16 :: Int -> Word16
intToWord16 n
  | n < 0   = error "Value too small for Word8"
  | n > 65535 = error "value too big for Word8"
  | otherwise = (fromIntegral n)
  
intToWord32 :: Int -> Word32
intToWord32 n
  | n < 0   = error "Value too small for Word8"
  | n > 4294967295 = error "value too big for Word8"
  | otherwise = (fromIntegral n)


intToWord64 :: Int -> Word64
intToWord64 n
  | n < 0   = error "Value too small for Word8"
  | otherwise = (fromIntegral n)