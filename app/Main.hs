module Main where


import Prelude hiding (readFile)
import Lib
import System.Directory
import Data.ByteString hiding (map)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Search

books = [ "76-0.txt", "74-0.txt" ]

singleWords :: [ByteString]
singleWords = map C8.pack ["Huckleberry", "Huck", "Tom", "Sawyer", "Aunt", "Polly"]

main :: IO ()
main = do
  getHomeDirectory >>= setCurrentDirectory
  makeAbsolute "./Downloads" >>= setCurrentDirectory
  book <- readFile "76-0.txt"

  print "Searching for Huckleberry..."
  --let idx = indices (C8.pack "Huckleberry") book
  let idx = boyerSearchOne (C8.pack "Huckleberry") book
  print idx


  print "Searching for some words..."
  let idxs = boyerSearchMany singleWords book
  print idxs
