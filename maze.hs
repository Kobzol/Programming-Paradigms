module Maze where

import Prelude

type Point = (Int, Int)
type Maze = [String]
type Path = (Point, Int)

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


get :: Maze -> Point -> Char
get m (x, y) = (m !! x) !! y

isWall :: Maze -> Point -> Bool
isWall m p = (get m p) == '#'

isValid :: Maze -> Point -> Bool
isValid m (x, y) = if x < 0 || y < 0 || x >= length m || y >= length m then False
                   else True
               
setWallInner :: String -> Int -> String
setWallInner x i = (take i x) ++ "#" ++ (drop (i + 1) x)
                   
setWall :: Maze -> Point -> Maze
setWall m (x, y) = (take x m) ++ [(setWallInner (m !! x) y)] ++ (drop (x + 1) m)

--steps2 :: Maze -> Point -> Point -> Int
--steps2 m (fX, fY) (tX, tY) =
  --if from == to then 0
  --else if not (isValid m from) then 10000
  --else if isWall m from then 10000
  --else (minimum distances) + (steps2 markedMaze minPoint to)
  --where
    --from = (fX, fY)
    --to = (tX, tY)
    --up = (fX, fY - 1)
    --down = (fX, fY + 1)
    --right = (fX + 1, fY)
    --left = (fX - 1, fY)
    --markedMaze = setWall m from
    --distances = [(steps2 markedMaze up to), (steps2 markedMaze down to), (steps2 markedMaze left to), (steps2 markedMaze right to)]


inSolved :: Point -> [Path] -> Bool
inSolved point xs = length [p | (p, c) <- xs, point == p] > 0

neighbour :: Point -> Int -> [Path]
neighbour (p1, p2) price = [
  ((p1 + 1, p2), price + 1),
  ((p1 - 1, p2), price + 1),
  ((p1, p2 + 1), price + 1),
  ((p1, p2 - 1), price + 1)]
                  
find :: [Path] -> Maze -> [Path] -> [Path]
find [] _ x = x
find (((p1, p2), cost):xs) m solved =
  if free && notSolved then find (xs ++ (neighbour point cost)) m ((point, cost):solved)
  else find xs m solved
  where
    point = (p1, p2)
    free = not (isWall m point)
    notSolved = not (inSolved point solved)

steps :: Maze -> Point -> Point -> Int
steps m start end = head [price | (p, price) <- path, p == end]
  where
    path = find [(start, 0)] m []
    
    
createPath :: [Path] -> Path -> [Path]
createPath [] path = []
createPath ((px, costx):xs) (point, cost) =
  if cost == (costx + 1) then ((px, costx) : createPath xs (px, costx))
  else createPath xs (px, costx)
    
path :: Maze -> Point -> Point -> [Point]
path m start end = [p | (p, c) <- reverse $ x : (createPath xs x)]
  where
    (x:xs) = find [(start, 0)] m []