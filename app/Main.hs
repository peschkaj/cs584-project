module Main where


--import Prelude hiding (readFile)
import Lib
import System.Directory
import Data.ByteString as B hiding (map, take)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Search
import Data.List
import Criterion.Main

books :: [String]
books = [ "76-0.txt", "74-0.txt" ]

singleWords :: [ByteString]
singleWords = sortOn B.length w
  where w = map C8.pack ["Huckleberry", "Huck", "Tom", "Sawyer", "Aunt",
                           "Polly", "sword", "hamlet", "boy", "Jim", "circus",
                           "handle", "food", "hunger", "powerful", "poem",
                           "Moses", "Finn", "Pap", "Judge", "Thatcher", "ghost",
                           "raft", "pole", "river", "fish", "line", "jug", "pig",
                           "shoat", "shack", "revival", "tent", "bench", "dress",
                           "bonnet", "horse", "town", "old", "young", "man",
                           "speech", "sing", "duke", "time", "crick", "funeral",
                           "hand", "bank", "cave", "summer", "sleep", "white",
                           "shirt", "collar", "thread", "wagon", "drunk"]

searchHuck :: ByteString -> [Int]
searchHuck = boyerSearchOne (C8.pack "Huckleberry")

search5 :: ByteString -> [Int]
search5 = boyerSearchMany (take 5 singleWords)

search10 :: ByteString -> [Int]
search10 = boyerSearchMany (take 10 singleWords)

search15 :: ByteString -> [Int]
search15 = boyerSearchMany (take 15 singleWords)

search20 :: ByteString -> [Int]
search20 = boyerSearchMany (take 20 singleWords)

searchMany :: ByteString -> [Int]
searchMany = boyerSearchMany singleWords

krSO :: ByteString -> [(Int, [Int])]
krSO = krSearchOne (C8.pack "Huckleberry")

krSM5 :: ByteString -> [(Int, [Int])]
krSM5 = krSearchMany (take 5 singleWords)

krSM10 :: ByteString -> [(Int, [Int])]
krSM10 = krSearchMany (take 10 singleWords)

krSM15 :: ByteString -> [(Int, [Int])]
krSM15 = krSearchMany (take 15 singleWords)

krSM20 :: ByteString -> [(Int, [Int])]
krSM20 = krSearchMany (take 20 singleWords)

krSM :: ByteString -> [(Int, [Int])]
krSM = krSearchMany singleWords

main :: IO ()
main = do
  getHomeDirectory >>= setCurrentDirectory
  makeAbsolute "./Downloads" >>= setCurrentDirectory
  book <- B.readFile "76-0.txt"
  defaultMain
    [ bgroup
        "boyer-moore"
        [ bench "searchOne" $ whnf searchHuck book
        , bench "search5" $ whnf search5 book
        , bench "search10" $ whnf search10 book
        , bench "search15" $ whnf search15 book
        , bench "search20" $ whnf search20 book
        , bench "searchMany" $ whnf searchMany book
        ]
    , bgroup
        "karp-rabin"
        [ bench "krSearchOne" $ whnf krSO book
        , bench "krSM5" $ whnf krSM5 book
        , bench "krSM10" $ whnf krSM10 book
        , bench "krSM15" $ whnf krSM15 book
        , bench "krSM20" $ whnf krSM20 book
        , bench "krSearchMany" $ whnf krSM book
        ]
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
