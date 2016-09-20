module Cviko2 where

import Prelude
import Data.Char -- toUpper

-- x ++ y má složitost O(len(x))
-- concat ? jako flatten

-- List comprehensions
x = [n | n <- [1..10], n `mod` 2 == 0]

pow :: Num a => [a] -> [a]
pow xs = [x * x | x <- xs]

_toUpper :: String -> String
_toUpper xs = [ toUpper x | x <- xs]

delitele :: Int -> [Int]
delitele n = [x | x <- [1..n], n `mod` x == 0]

type Person = String
type Book = String
type Database = [ (Person, Book)]

d1 = [("Marek", "Dedecek"), ("Marek", "Haskell"), ("Jana", "Haskell")]

books :: Database -> Person -> [Book]
books db p = [book | (loaner, book) <- db, loaner == p]

borrowers :: Database -> Book -> [Person]
borrowers db b = [loaner | (loaner, book) <- db, book == b]

borrowed :: Database -> Book -> Bool
borrowed db b = length ([loaner | (loaner, book) <- db, book == b]) > 0

numBorrowed :: Database -> Book -> Int
numBorrowed db b = sum [1 | (loaner, book) <- db, book == b]

makeLoan :: Database -> Person -> Book -> Database
makeLoan db p b = (p, b) : db

returnLoan :: Database -> Person -> Book -> Database
returnLoan db p b = [(loaner, book) | (loaner, book) <- db, (loaner /= p || book /= b)]

type Pic = [String]

pp :: Pic -> IO ()  -- monáda
pp = putStr . concat . map (++"\n")

obr :: Pic
obr = [
  "....#....",
  "...###...",
  "..#.#.#..",
  ".#..#..#.",
  "....#....",
  "....#....",
  "....#####"
  ]

flipV :: Pic -> Pic
flipV p = reverse p

flipH :: Pic -> Pic
flipH p = [reverse line | line <- p]

above :: Pic -> Pic -> Pic
above p1 p2 = p1 ++ p2

nextTo :: Pic -> Pic -> Pic
nextTo p1 p2 = zipWith (++) p1 p2

rotateLeft :: Pic -> Pic
rotateLeft [] = []
rotateLeft p = flipV (rotateLeftInner p)
  where
    rotateLeftInner [] = []
    rotateLeftInner p = (returnFirst p) : rotateLeftInner [xs | (x:xs) <- p]

rotateRight :: Pic -> Pic
rotateRight [] = []
rotateRight p = (reverse (returnFirst p)) : rotateRight [xs | (x:xs) <- p]

returnFirsts :: [[a]] -> [a] -> [a]
returnFirsts [] x = x
returnFirsts (x:xs) y = if null x then y
                        else returnFirsts xs ((head x) : y)

returnFirst x = reverse (returnFirsts x [])

-- lepší rešení
sloupec [] = []
sloupec (x:xs) = [x] : sloupec xs

rotLeft :: Pic -> Pic
rotLeft [x] = reverse (sloupec x)
rotLeft (x:xs) = (sloupec x) `nextTo` (rotLeft xs)

rotRight :: Pic -> Pic
rotRight [x] = sloupec x
rotRight (x:xs) = (rotRight xs) `nextTo` (sloupec x)



