module Du3 where

import Prelude
import Control.Exception.Base
import Debug.Trace
import Data.Array
import Data.Ix

maxLineLength = 6
--vstup = "In computer science, \tfunctional programming is a programming \tparadigm that treats computation   as \tthe evaluation of mathematical functions and avoids state and mutable data.    It emphasizes the application of functions, in contrast to the imperative \tprogramming style, which emphasizes changes in state."
vstup = "aaa bb cc ddddd"

-- dynamické programování - rozdelit na co nejmenší pocet rádku

repeatItem :: a -> Int -> [a]
repeatItem a count = take count (repeat a)

lineLength :: [String] -> Int -> Int -> Int
lineLength line spaceWidth maxLength =
  if length line == 0 then 0
  else wordLength + safeWordLength
  where
    wordLength = sum (map (length) line)
    safeWords = init line
    safeWordLength = spaceWidth * (length safeWords)

lineLengthWithSpaces :: [String] -> Int -> Int -> Int
lineLengthWithSpaces line spaceWidth maxLength =
  if length line == 0 then 0
  else startLength + appendLength
  where
    wordLength = sum (map (length) line)
    safeWords = init line
    safeWordLength = spaceWidth * (length safeWords)
    startLength = wordLength + safeWordLength
    appendLength = max 0 (min (maxLength - (startLength)) spaceWidth)

makeSpaces :: [String] -> Int -> [String]
makeSpaces line spaceWidth = (addSpace (init line)) ++ [last line]
  where
    addSpace [] = []
    addSpace (x:xs) = x:((repeatItem ' ' spaceWidth): addSpace xs)

justifyLine :: [String] -> Int -> Int -> String
justifyLine line spaceWidth maxLength =
  if (length line) == 1 then head line
  else if (length line) == 0 then ""
  else increaseSpace lineWithSpaces 1 spaceWidth
  where
    lineWithSpaces = makeSpaces line spaceWidth
    realLength l = length $ concat l
    increaseSpace line i space =
      if (realLength line) >= maxLength then concat line
      else if i >= (length line) then increaseSpace line 1 (space + 1)
      else increaseSpace widenedLine (i + 2) space
      where
        widenedLine = (take i line) ++ [repeatItem ' ' (space + 1)] ++ (drop (i + 1) line)

widenLine :: [String] -> Int -> Int -> String
widenLine line spaceWidth maxLength = justifyLine line 0 maxLength

makeLine :: [String] -> Int -> Int -> String
makeLine line spaceWidth maxLength = take maxLength $ concat (map (++ (repeatItem ' ' spaceWidth)) line)

lineify :: [String] -> String
lineify s = concat (map (++ "\n") s)


greedyJustify :: [String] -> [String] -> [String] -> Int -> [String]
greedyJustify [] line res maxLength = res ++ [(widenLine line 1 maxLength)]
greedyJustify (word:ls) line res maxLength =
  if appendedLength < maxLength then greedyJustify ls appended res maxLength
  else if appendedLength > maxLength then greedyJustify (word:ls) [] (res ++ [widenedLine]) maxLength
  else greedyJustify ls [] (res ++ [widenedAppended]) maxLength
  where
    appended = line ++ [word]
    appendedLength = lineLength appended 1 maxLength
    widenedLine = widenLine line 1 maxLength
    widenedAppended = widenLine appended 1 maxLength


splitWordAt :: String -> Int -> (String, String)
splitWordAt s i = ((take i s), (drop i s))

hyphenateJustify :: [String] -> [String] -> [String] -> Int -> [String]
hyphenateJustify [] line res maxLength = res ++ [(makeLine line 1 maxLength)]
hyphenateJustify (word:ls) line res maxLength =
  if appendedLength < maxLength then hyphenateJustify ls appended res maxLength
  else if (appendedLength > maxLength) && (length leftHalf > 0) then hyphenateJustify (rightHalf:ls) [] (res ++ [widenedLineHyphen]) maxLength
  else if appendedLength > maxLength then hyphenateJustify (word:ls) [] (res ++ [widenedLine]) maxLength
  else hyphenateJustify ls [] (res ++ [widenedAppended]) maxLength
  where
    appended = line ++ [word]
    appendedLength = lineLength appended 1 maxLength
    widenedLine = widenLine line 1 maxLength
    widenedLineHyphen = widenLine (line ++ [leftHalf ++ "-"]) 1 maxLength
    widenedAppended = widenLine appended 1 maxLength
    (leftHalf, rightHalf) = splitWordAt word (((length word) - (appendedLength - maxLength)) - 1)


scoreLines :: [String] -> Int
scoreLines l = sum (map (\x -> scoreSquare (scoreLine x)) l)
  where
    scoreSquare x = x * x
    scoreLine l = length (takeWhile (\x -> x == ' ') (reverse l))

backtrackJustify :: [String] -> [String] -> [String] -> Int -> [String]
backtrackJustify [] line res maxLength = res ++ [(makeLine line 1 maxLength)]
backtrackJustify (word:ls) line res maxLength =
  if (appendedLength < maxLength) && (length line > 0) && (maxLength - appendedLength < (10)) then
    if scoreKeep < scoreMove then linesKeep
    else linesMove
  else if appendedLength < maxLength then linesKeep
  else if appendedLength > maxLength then linesMove
  else greedyJustify ls [] (res ++ [widenedAppended]) maxLength
  where
    appended = line ++ [word]
    appendedLength = lineLength appended 1 maxLength
    widenedLine = widenLine line 1 maxLength
    widenedAppended = widenLine appended 1 maxLength
    linesKeep = backtrackJustify ls appended res maxLength
    linesMove = backtrackJustify (word:ls) [] (res ++ [widenedLine]) maxLength
    scoreKeep = scoreLines (linesKeep)
    scoreMove = scoreLines (linesMove)

takeRange :: [a] -> Int -> Int -> [a]
takeRange l i j = (take (j-i) (drop i l))

badness :: [String] -> Int -> Int -> Int -> Int
badness w i j maxLength = badnessScore (takeRange w i j)
  where
    badnessScore :: [String] -> Int
    badnessScore [] = 0
    badnessScore w = if len > maxLength then 10000000
                     else margin * margin
      where
        len = lineLength w 1 maxLength
        margin = maxLength - len

memoD :: [String] -> Int -> Array Int (Int,Int)
memoD w maxLength = memos
  where
    innerMemo i =
      if i == len then (len, 0)
      else (fst (head (filter (\(i, bad) -> bad == minBadness) followingIndices)), minBadness)
      where
        minBadness = minimum (map snd followingIndices)
        followingIndices = [(j, (badness w i j maxLength) + snd (memos ! j)) | j <- [len, (len-1)..(i + 1)]]
    memos = listArray (0, len) [innerMemo i | i <- [0..len]]
    len = length w

texJustify :: [String] -> [String] -> [String] -> Int -> [String]
texJustify w l res maxLength = justifyInner [] 0
  where
    table = memoD w maxLength
    wordsLength = length w
    justifyInner :: [String] -> Int -> [String]
    justifyInner res i = if i >= wordsLength then res
                           else justifyInner (res ++ [line]) endIndex
      where
        endIndex = fst (table ! i)
        line = widenLine wds 1 maxLength
        wds = takeRange w i endIndex

du :: ([String] -> [String] -> [String] -> Int -> [String]) -> IO ()
du x = putStr (lineify (duRaw x))
duRaw x = x (words vstup) [] [] maxLineLength