module MazeMutable where

import Prelude
import Data.Array

type Point = (Int, Int)
type MazeBlock = (Char, Int)
type Maze = Array Point MazeBlock
type MazeInput = [String]

noPath = 1000

in1 = [
  "#####",
  "# # #",
  "#   #",
  "#####"]
  

inputToMaze :: MazeInput -> Maze
inputToMaze str = listArray ((0, 0), (width - 1, width - 1)) [(c, -1) | c <- concat str]
  where
    width = length str
    
    
isValid :: Point -> Int -> Bool
isValid (x, y) width = x >= 0 && x < width && y >= 0 && y < width 
    

findPath :: Maze -> Int -> Point -> Point -> Int -> Int
findPath m width from to currDist = 
  if not (isValid from width) then noPath 
  else if from == to then currDist
  else if c == '#' then noPath
  else
    minimum []
  where
    (c, dist) = m ! from
    do
      x <- [1..10]
    --(m ! from) = (c, currDist)
    --paths = [findPath m width p to (currDist + 1)]


steps :: MazeInput -> Point -> Point -> Int
steps m from to = findPath maze (length m) from to 0
  where
    maze = inputToMaze m