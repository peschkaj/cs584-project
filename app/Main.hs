{-# LANGUAGE BangPatterns #-}
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

{-
singleWords :: [ByteString]
singleWords = map C8.pack ["screw","successful","chin","adamant","tick","enchanted","wry","shaggy","panoramic","squash","soothe","time","stocking","melt","hushed","nod","needy","person","zealous","reaction","economic","macabre","public","cats","manage","tasteless","peaceful","unarmed","aberrant","leg","curvy","gamy","talented","abortive","value","whole","word","colour","hope","amusing","vest","discussion","birthday","sick","surprise","superficial","language","feeble","general","test","wail","gigantic","teeny","unbecoming","jittery","craven","quickest","carpenter","pleasant","dazzling","tart","nebulous","destruction","cooing","muddle","hard-to-find","silly","blot","nose","son","regret","quince","orange","whip","marble","sneeze","excite","cap","eggnog","pizzas","conscious","water","vengeful","fuel","things","previous","teaching","unusual","concerned","numerous","fill","rod","nail","payment","addicted","bone","bite-sized","brash","noisy","force"]

search1 :: ByteString -> [Int]
search1 = boyerSearchOne $! Data.List.head singleWords

search2 :: ByteString -> [Int]
search2 = boyerSearchMany $! take 2 singleWords

search3 :: ByteString -> [Int]
search3 = boyerSearchMany $! take 3 singleWords

search4 :: ByteString -> [Int]
search4 = boyerSearchMany $! take 4 singleWords

search5 :: ByteString -> [Int]
search5 = boyerSearchMany $! take 5 singleWords

searchMany :: ByteString -> [Int]
searchMany = boyerSearchMany singleWords

krSO :: ByteString -> [(Int, [Int])]
krSO = krSearchOne $! Data.List.head singleWords

krSM2 :: ByteString -> [(Int, [Int])]
krSM2 = krSearchMany $! take 2 singleWords

krSM3 :: ByteString -> [(Int, [Int])]
krSM3 = krSearchMany $! take 3 singleWords

krSM4 :: ByteString -> [(Int, [Int])]
krSM4 = krSearchMany $! take 4 singleWords

krSM5 :: ByteString -> [(Int, [Int])]
krSM5 = krSearchMany $! take 5 singleWords

krSM :: ByteString -> [(Int, [Int])]
krSM = krSearchMany singleWords
-}

main :: IO ()
main = do
  let !singleWords = map C8.pack ["screw","successful","chin","adamant","tick","enchanted","wry","shaggy","panoramic","squash","soothe","time","stocking","melt","hushed","nod","needy","person","zealous","reaction","economic","macabre","public","cats","manage","tasteless","peaceful","unarmed","aberrant","leg","curvy","gamy","talented","abortive","value","whole","word","colour","hope","amusing","vest","discussion","birthday","sick","surprise","superficial","language","feeble","general","test","wail","gigantic","teeny","unbecoming","jittery","craven","quickest","carpenter","pleasant","dazzling","tart","nebulous","destruction","cooing","muddle","hard-to-find","silly","blot","nose","son","regret","quince","orange","whip","marble","sneeze","excite","cap","eggnog","pizzas","conscious","water","vengeful","fuel","things","previous","teaching","unusual","concerned","numerous","fill","rod","nail","payment","addicted","bone","bite-sized","brash","noisy","force"]

  let !search1 = boyerSearchOne $! Data.List.head singleWords
  let !search2 = boyerSearchMany $! take 2 singleWords
  let !search3 = boyerSearchMany $! take 3 singleWords
  let !search4 = boyerSearchMany $! take 4 singleWords
  let !search5 = boyerSearchMany $! take 5 singleWords
  let !searchMany = boyerSearchMany singleWords
  let !krSO = krSearchOne $! Data.List.head singleWords
  let !krSM2 = krSearchMany $! take 2 singleWords
  let !krSM3 = krSearchMany $! take 3 singleWords
  let !krSM4 = krSearchMany $! take 4 singleWords
  let !krSM5 = krSearchMany $! take 5 singleWords
  let !krSM = krSearchMany singleWords

  getHomeDirectory >>= setCurrentDirectory
  makeAbsolute "./Downloads" >>= setCurrentDirectory
  book1 <- B.readFile "76-0.txt"
  book2 <- B.readFile "2600-0.txt"
  defaultMain
    [ bgroup
        "boyer-moore/HuckFinn"
        [ bench "search1" $ whnf search1 book1
        , bench "search2" $ whnf search2 book1
        , bench "search3" $ whnf search3 book1
        , bench "search4" $ whnf search4 book1
        , bench "search5" $ whnf search5 book1
        , bench "searchMany" $ whnf searchMany book1
        ]
    , bgroup
        "karp-rabin/HuckFinn"
        [ bench "krSearchOne" $ whnf krSO book1
        , bench "krSM2" $ whnf krSM2 book1
        , bench "krSM3" $ whnf krSM3 book1
        , bench "krSM4" $ whnf krSM4 book1
        , bench "krSM5" $ whnf krSM5 book1
        , bench "krSearchMany" $ whnf krSM book1
        ]
    , bgroup
        "boyer-moore/WarAndPeace"
        [ bench "search1" $ whnf search1 book2
        , bench "search2" $ whnf search2 book2
        , bench "search3" $ whnf search3 book2
        , bench "search4" $ whnf search4 book2
        , bench "search5" $ whnf search5 book2
        , bench "searchMany" $ whnf searchMany book2
        ]
    , bgroup
        "karp-rabin/WarAndPeace"
        [ bench "krSearchOne" $ whnf krSO book2
        , bench "krSM2" $ whnf krSM2 book2
        , bench "krSM3" $ whnf krSM3 book2
        , bench "krSM4" $ whnf krSM4 book2
        , bench "krSM5" $ whnf krSM5 book2
        , bench "krSearchMany" $ whnf krSM book2
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
