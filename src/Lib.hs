module Lib
    ( --readBooks
      readBook
    ) where

import Prelude hiding (readFile)
import Data.ByteString
import Data.String.Utils



readBook :: String -> IO ByteString
readBook filename = readFile filename
