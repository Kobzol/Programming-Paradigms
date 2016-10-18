module MazeMutable where

import Prelude
import Data.Array
import Control.Monad.ST
import Data.Array.ST
import Control.Monad (forM_)
import Data.Array.Unsafe

type Point = (Int, Int)
type MazeBlock = (Char, Int)
type Maze = Array Int MazeBlock
type MazeInput = [String]

noPath = 1000

in1 = [
  "##################################################",
  "#           ######                               #",
  "# ### ############################# ### ##### ####",
  "# ### #########                    #     ##### ###",
  "# ### ######### #######################       ####",
  "# ###    ###### ####################### ##########",
  "# #### ######## ####################### ##########",
  "# #### ######## ####################### ##########",
  "#       ###         ###################    #######",
  "####### ### ####### ###################### #######",
  "####### ##  ####### ###################### #######",
  "##          ####### ###################### #######",
  "####### ### #######                ####### #######",
  "####### ### ########### ################## #######",
  "#######     ########### ################## #######",
  "####################### ################## #######",
  "#                                  ####### #######",
  "####################### ################## #######",
  "####################### ################## #######",
  "####################### ################## #######",
  "#######################    ############### #######",
  "########################## ############### #######",
  "####################### ## ############### #######",
  "####################### ## ###############    ####",
  "####################### ## ################## ####",
  "####################### ## ################## ####",
  "#######################      ################ ####",
  "############################ ################ ####",
  "############################ ################ ####",
  "############################ ################ ####",
  "############################ ################ ####",
  "############################ ################ ####",
  "############################ ################ ####",
  "###########################                   ####",
  "############################ ################ ####",
  "############################     ############ ####",
  "################################ ############ ####",
  "################################ ############ ####",
  "################################      ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "##################################### ####### ####",
  "#####################################     ### ####",
  "#########################################     ####",
  "##################################################"]
  
test m = runSTArray $ do
  arr <- newArray (0,1) 5
  while True



inputToMutMaze :: MazeInput -> Maze
inputToMutMaze m = 
  runSTArray $ do
    arr <- newArray (0, width) ('#', -1)
    forM_ [0..width] $ \i -> writeArray arr i ((str !! i), -1)
    return arr
  where
    str = concat m
    width = (length str) - 1

inputToMaze :: MazeInput -> Maze
inputToMaze str = listArray (0, width * width - 1) [(c, -1) | c <- concat str]
  where
    width = length str
    
    
isValid :: Point -> Int -> Bool
isValid (x, y) width = x >= 0 && x < width && y >= 0 && y < width

getNeighbours :: Point -> [Point]
getNeighbours (x, y) = [(x - 1, y),
                        (x + 1, y),
                        (x, y - 1),
                        (x, y + 1)] 


updateMaze :: Maze -> Int -> Int -> MazeBlock -> Maze
updateMaze m i width block = runSTArray $ do
  --arr <- newArray (0, (width*width - 1)) ('#', -1)
  --forM_ [0..(width*width - 1)] $ \i -> writeArray arr i (m ! i)
  arr <- unsafeThaw m
  writeArray arr i block
  return arr    

findPath :: Maze -> Int -> Point -> Point -> Int -> Int
findPath m width from to currDist = 
  if not (isValid from width) then noPath 
  else if c == '#' then noPath
  else if from == to then currDist
  else
    minimum distances
  where
    (x, y) = from
    indexPoint = (x * width + y)
    (c, dist) = m ! indexPoint
    updatedMaze = updateMaze m indexPoint width ('#', currDist) --m // [(indexPoint, ('#', currDist))]
    distances = [findPath updatedMaze width neighbour to (currDist + 1) | neighbour <- getNeighbours from]

steps :: MazeInput -> Point -> Point -> Int
steps m from to = findPath maze (length m) from to 0
  where
    maze = inputToMaze m