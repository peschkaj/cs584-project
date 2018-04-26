module Main where

import Lib
import System.Directory
import Data.ByteString.Char8 (pack)
import Data.ByteString.Search

main :: IO ()
main = do
  getHomeDirectory >>= setCurrentDirectory
  makeAbsolute "./Downloads" >>= setCurrentDirectory
  book <- readBook "76-0.txt"

  let idx = indices (pack "Huckleberry") book

  print idx

  putStrLn "suck it"
