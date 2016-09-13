module Du1 where

import Prelude hiding (length, zip, zipWith, reverse, min)

-- 1. length of a list
length :: [t] -> Int
length [] = 0
length (x:xs) = 1 + length xs


-- 2. merge two lists
merge :: [a] -> [a] -> [a]
merge [] x = x
merge (x:xs) y = x : merge xs y


-- 3. merge two lists into a list of tuples
zip :: [a] -> [b] -> [(a, b)]
zip [] x = []
zip x [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipTuple :: [a] -> [b] -> [(a, b)]
zipTuple x y = zipWith makeTuple x y
  where
    makeTuple x y = (x, y)


-- 4. reverse a list
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


-- 5. merge a list with a function
zipWith :: (a -> b -> t) -> [a] -> [b] -> [t]
zipWith f [] x = []
zipWith f x [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


-- 6. dot product
dot :: (Num a) => [a] -> [a] -> a
dot [] [] = 0
dot (x:xs) (y:ys) = (x * y) + dot xs ys    


-- 7. cartesian product of two vectors
cartesian :: (Num a) => [a] -> [a] -> [(a, a)]
cartesian [] x = []
cartesian (x:xs) ys = cartesianSingle x ys ++ cartesian xs ys
  where
      cartesianSingle :: (Num a) => a -> [a] -> [(a, a)]
      cartesianSingle el [] = []
      cartesianSingle el (y:ys) = (el, y) : cartesianSingle el ys


-- 8. find smallest element in list
min :: (Ord a, Num a) => [a] -> a
min [] = 0
min (x:xs) = minInner x xs
  where
    minInner :: (Ord a) => a -> [a] -> a
    minInner x [] = x
    minInner x (y:ys) = if y < x then minInner y ys
                        else minInner x ys