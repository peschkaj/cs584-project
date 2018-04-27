module Main where


import Prelude hiding (readFile)
import Lib
import System.Directory
import Data.ByteString hiding (map)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Search
import Criterion.Main

books = [ "76-0.txt", "74-0.txt" ]

singleWords :: [ByteString]
singleWords = map C8.pack ["Huckleberry", "Huck", "Tom", "Sawyer", "Aunt",
                           "Polly", "sword", "hamlet", "boy", "Jim", "circus",
                           "handle", "food", "hunger", "powerful", "poem",
                           "Moses", "Finn", "Pap", "Judge", "Thatcher", "ghost",
                           "raft", "pole", "river", "fish", "line", "jug", "pig",
                           "shoat", "shack", "revival", "tent", "bench", "dress",
                           "bonnet", "horse", "town", "old", "young", "man",
                           "speech", "sing", "duke", "time", "crick", "funeral",
                           "hand", "bank", "cave", "summer", "sleep", "white",
                           "shirt", "collar", "thread", "wagon", "drunk"]

searchHuck = boyerSearchOne (C8.pack "Huckleberry")

searchMany = boyerSearchMany singleWords

krSM = krSearchMany singleWords

main :: IO ()
main = do
  getHomeDirectory >>= setCurrentDirectory
  makeAbsolute "./Downloads" >>= setCurrentDirectory
  book <- readFile "76-0.txt"

  defaultMain [
    bgroup "boyer-moore" [ bench "searchOne" $ whnf searchHuck book
                         , bench "searchMany" $ whnf searchMany book
                         , bench "krSearchMany" $ whnf krSM book ]
              ]

  {-
  print "Searching for Huckleberry..."
  --let idx = indices (C8.pack "Huckleberry") book
  let idx = boyerSearchOne (C8.pack "Huckleberry") book
  print idx


  print "Searching for some words..."
  let idxs = boyerSearchMany singleWords book
  print idxs
  -}
