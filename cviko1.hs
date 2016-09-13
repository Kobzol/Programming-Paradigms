-- skladani funkci: rotate = flipH . flipV
-- .hs → skript, .lhs → vše je poznámka kromě >

-- Nazvy
-- [a-z]identifikator - promenne, funkce
-- [A-Z]identifikator - typ, konstruktor

-- Moduly
-- implicitně je naimportovaný modul Prelude
-- import Prelude hiding (max, min)

-- Datové typy
-- data Color = White | Black
-- data Tree a = Leaf a | Node a (Tree a) (Tree a)
-- type String = [Char]
-- type table a = [(String, a)]

-- Pojmenované vzory
-- duphd @p(x:xs) = x:p

-- Placeholder
-- fn _ = ...

module Cviko1 where

import Prelude hiding (max, gcd)

square :: Int -> Int
square x = x * x

soucet :: Int -> Int -> Int
soucet x y = x + y

max :: Int -> Int -> Int
max x y
  | x >= y = x
  | otherwise = y

faktIf :: Int -> Int  
faktIf n =  if n == 0 then 1
            else n * faktIf (n - 1)

fibStep :: (Int, Int) -> (Int, Int)
fibStep (u, v) = (v, u + v)
fibPair :: Int -> (Int, Int)
fibPair n
  | n == 0 = (0, 1)
  | otherwise = fibStep (fibPair(n - 1))
fib :: Int -> Int
fib = fst . fibPair

fibSlow :: Int -> Int
fibSlow 0 = 0
fibSlow 1 = 1
fibSlow n = fib (n - 2) + fib (n - 1)

gcd :: Int -> Int -> Int
gcd x 0 = x
gcd x y = gcd y (x `mod` y)

gcdSlow :: Int -> Int -> Int
gcdSlow x y
    | x == y = x
    | x < y = gcdSlow x (y - x)
    | otherwise = gcdSlow (x - y) y

len :: [t] -> Int
len [] = 0
len (_:xs) = 1 + len xs