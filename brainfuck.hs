module Brainfuck where

import Prelude
import Data.Char
import Data.List

type Runtime  = (Int, [Int]) -- ptr, memory

set :: Int -> [Int] -> Int -> [Int]
set ptr mem value = addInner 0 mem
  where
    addInner i [] = []
    addInner i (m:ms) = if i == length mem then []
                        else if i == ptr then (value) : addInner (i + 1) ms
                        else m : addInner (i + 1) ms
                        
add :: Int -> [Int] -> Int -> [Int]
add ptr mem value = set ptr mem ((mem !! ptr) + value)
                        
getNextEnd :: String -> Int -> Int
getNextEnd program i = (+) 1 $ head $ elemIndices ']' (drop i program)

getPreviousBegin :: String -> Int -> Int
getPreviousBegin program i = (+) 1 $ head $ elemIndices '[' (take i program)

run :: String -> String -> String
run program input = runInner 0 input (0, (take 256 $ repeat 0))
  where
    runInner :: Int -> String -> Runtime -> String
    runInner inst input (ptr, mem) = 
      if inst == length program then []
      else if i == '>' then runInner nextInst input (ptr + 1, mem)
      else if i == '<' then runInner nextInst input (ptr - 1, mem)
      else if i == '+' then runInner nextInst input (ptr, add ptr mem 1)
      else if i == '-' then runInner nextInst input (ptr, add ptr mem (-1))
      else if i == '.' then (chr (mem !! ptr)) : runInner nextInst input (ptr, mem)
      else if i == ',' then runInner nextInst (tail input) (ptr, set ptr mem (ord inputChar))
      else if i == '[' then 
        if dat == 0 then runInner (getNextEnd program inst) input (ptr, mem)
        else runInner nextInst input (ptr, mem)
      else if i == ']' then
        if dat /= 0 then runInner (getPreviousBegin program inst) input (ptr, mem)
        else runInner nextInst input (ptr, mem)
      else runInner nextInst input (ptr, mem)
      where
        i = program !! inst
        nextInst = inst + 1
        dat = mem !! ptr
        inputChar = head input
        
