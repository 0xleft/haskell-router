module Router.Util (
  intToWord8,
) where

import Data.Word (Word8)

intToWord8 :: Int -> Word8
intToWord8 n
  | n < 0   = error "Value too small for Word8"
  | n > 255 = error "value too big for Word8"
  | otherwise = (fromIntegral n)