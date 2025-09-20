{-# LANGUAGE ExistentialQuantification #-}

module Router.Types (
  Packable(..),
) where

import qualified Data.ByteString.Char8 as B
import Data.Data (Data)

class Packable p where
  pack :: p -> B.ByteString