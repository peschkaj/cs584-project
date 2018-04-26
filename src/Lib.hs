module Lib
    ( boyerSearchOne
    , boyerSearchMany
    ) where

import Data.ByteString hiding (foldr)
import Data.ByteString.Search


boyerSearchOne :: ByteString -- ^ The pattern to find
               -> ByteString -- ^ The filename to search
               -> [Int]
boyerSearchOne = indices


boyerSearchMany :: [ByteString]   -- ^ The patterns to find
                -> ByteString     -- ^ The filename to search
                -> [Int]
boyerSearchMany ss b = foldr (\item acc -> acc ++ boyerSearchOne item b) [] ss
