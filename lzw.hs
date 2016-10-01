module LZE where

import Prelude
import Data.Char

type Dictionary = ([(String, Int)], Int)

initDict :: Dictionary
initDict = ([([chr i], i) | i <- [0..255]], 256)

isInDict :: Dictionary -> String -> Bool
isInDict (codes, size) s = length ([c | (c, i) <- codes, c == s]) > 0

getFromDictByStr :: Dictionary -> String -> Int
getFromDictByStr (codes, size) s = head [i | (c, i) <- codes, c == s]

getFromDictById :: Dictionary -> Int -> String
getFromDictById (codes, size) j = head [c | (c, i) <- codes, i == j]

addToDict :: Dictionary -> String -> Dictionary
addToDict (codes, size) s = ((s, size) : codes, size + 1)

compress :: String -> [Int]
compress s = compressInner s "" dict
  where
    dict = initDict
    compressInner :: String -> String -> Dictionary -> [Int]
    compressInner "" w dict = if w == "" then []
                              else [getFromDictByStr dict w]
    compressInner (x:xs) w dict =
      if isInDict dict wc then compressInner xs wc dict
      else (getFromDictByStr dict w) : compressInner xs [x] (addToDict dict wc)                            
      where
        wc = w ++ [x]
        
decompress :: [Int] -> String
decompress [] = ""
decompress (c:cs) = (chr c) : (decompressInner cs w dict)
  where
    dict = initDict
    w = [chr c] 
    decompressInner :: [Int] -> String -> Dictionary -> String
    decompressInner [] w dict = []
    decompressInner (c:cs) w dict = entry ++ decompressInner cs entry addedDict
      where
        entry = getFromDictById dict c
        addedDict = addToDict dict (w ++ [(head entry)])