{-# LANGUAGE BangPatterns #-}
module Main where

import Lib
import System.Directory
import Data.ByteString as B hiding (map, take)
import Data.ByteString.Char8 as C8 (pack)
import Data.List
import Criterion.Main

main :: IO ()
main = do
  let !singleWords = map C8.pack ["screw","successful","chin","adamant","tick","enchanted","wry","shaggy","panoramic","squash","soothe","time","stocking","melt","hushed","nod","needy","person","zealous","reaction","economic","macabre","public","cats","manage","tasteless","peaceful","unarmed","aberrant","leg","curvy","gamy","talented","abortive","value","whole","word","colour","hope","amusing","vest","discussion","birthday","sick","surprise","superficial","language","feeble","general","test","wail","gigantic","teeny","unbecoming","jittery","craven","quickest","carpenter","pleasant","dazzling","tart","nebulous","destruction","cooing","muddle","hard-to-find","silly","blot","nose","son","regret","quince","orange","whip","marble","sneeze","excite","cap","eggnog","pizzas","conscious","water","vengeful","fuel","things","previous","teaching","unusual","concerned","numerous","fill","rod","nail","payment","addicted","bone","bite-sized","brash","noisy","force"]

  let search1 = boyerSearchOne $ Data.List.head singleWords
  let search2 = boyerSearchMany $ take 2 singleWords
  let search3 = boyerSearchMany $ take 3 singleWords
  let search4 = boyerSearchMany $ take 4 singleWords
  let search5 = boyerSearchMany $ take 5 singleWords
  let searchMany = boyerSearchMany $ take 10 singleWords
  let krSO = krSearchOne $ Data.List.head singleWords
  let krSM2 = krSearchMany $ take 2 singleWords
  let krSM3 = krSearchMany $ take 3 singleWords
  let krSM4 = krSearchMany $ take 4 singleWords
  let krSM5 = krSearchMany $ take 5 singleWords
  let krSM = krSearchMany $ take 10 singleWords

  book1 <- B.readFile "76-0.txt"
  book2 <- B.readFile "2600-0.txt"
  defaultMain
    [ bgroup
        "boyer-moore/HuckFinn"
        [ bench "search1" $ nf search1 book1
        , bench "search2" $ nf search2 book1
        , bench "search3" $ nf search3 book1
        , bench "search4" $ nf search4 book1
        , bench "search5" $ nf search5 book1
        , bench "searchMany" $ nf searchMany book1
        ]
    , bgroup
        "karp-rabin/HuckFinn"
        [ bench "krSearchOne" $ nf krSO book1
        , bench "krSM2" $ nf krSM2 book1
        , bench "krSM3" $ nf krSM3 book1
        , bench "krSM4" $ nf krSM4 book1
        , bench "krSM5" $ nf krSM5 book1
        , bench "krSearchMany" $ nf krSM book1
        ]
    , bgroup
        "boyer-moore/WarAndPeace"
        [ bench "search1" $ nf search1 book2
        , bench "search2" $ nf search2 book2
        , bench "search3" $ nf search3 book2
        , bench "search4" $ nf search4 book2
        , bench "search5" $ nf search5 book2
        , bench "searchMany" $ nf searchMany book2
        ]
    , bgroup
        "karp-rabin/WarAndPeace"
        [ bench "krSearchOne" $ nf krSO book2
        , bench "krSM2" $ nf krSM2 book2
        , bench "krSM3" $ nf krSM3 book2
        , bench "krSM4" $ nf krSM4 book2
        , bench "krSM5" $ nf krSM5 book2
        , bench "krSearchMany" $ nf krSM book2
        ]
    ]
