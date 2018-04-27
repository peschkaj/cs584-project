module Lib
    ( boyerSearchOne
    , boyerSearchMany
    , krSearchMany
    ) where

import Data.ByteString hiding (foldr)
import Data.ByteString.Search
import Data.ByteString.Search.KarpRabin as KR


boyerSearchOne :: ByteString -- ^ The pattern to find
               -> ByteString -- ^ The filename to search
               -> [Int]
boyerSearchOne = indices


boyerSearchMany :: [ByteString]   -- ^ The patterns to find
                -> ByteString     -- ^ The filename to search
                -> [Int]
boyerSearchMany ss b = foldr (\item acc -> acc ++ boyerSearchOne item b) [] ss

krSearchMany :: [ByteString]
             -> ByteString
             -> [(Int, [Int])]
krSearchMany ss b = KR.indicesOfAny ss b
