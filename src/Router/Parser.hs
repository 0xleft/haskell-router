module Router.Parser () where

import qualified Data.ByteString.Char8 as B

parse :: B.ByteString -> ()
parse p = 
  let words = B.unpack p
  in ()