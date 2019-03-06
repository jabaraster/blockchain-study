module Sandbox where

import qualified Data.ByteString
import qualified Crypto.Hash.SHA256 as SHA256
  
main = print $ SHA256.hash (Data.ByteString.pack [0..255])