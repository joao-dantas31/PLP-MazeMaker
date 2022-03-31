module Utils where

import Data.HashSet
import System.Random

getPairFromTriplet :: (a, b, c) -> (a, b)
getPairFromTriplet (x, y, z) = (x, y)

get1st :: (a, b, c) -> a
get1st (x, _, _) = x

get2nd :: (a, b, c) -> b
get2nd (_, y, _) = y

get3rd :: (a, b, c) -> c
get3rd (_, _, z) = z

num :: (Integral a, Num b) => a -> b
num = fromIntegral

pairIntToNum :: (Num b1, Num b2) => (Int, Int) -> (b1, b2)
pairIntToNum (x, y) = (num x, num y)

getRandomNumTriplet :: StdGen -> (Int, Int, StdGen)
getRandomNumTriplet generator = (firstRand, sndRand, nextGen)
    where firstGen = next generator
          firstRand = fst firstGen
          secndGen = next $ snd firstGen
          sndRand = fst secndGen
          nextGen = snd secndGen

-- Given a pair of random numbers, the function mods them with gridSize
modRandomNumPair :: Int -> (Int, Int) -> (Int, Int)
modRandomNumPair gridSize (r, c) = ((r `mod` gridSize) + 1, (c `mod` gridSize) + 1)

-- Given the size of the list, get a random index
getRandomIndex :: StdGen -> Int -> (Int, StdGen)
getRandomIndex generator size = (modded, nextGen)
   where (randomNum, nextGen) = next generator
         modded = randomNum `mod` size

-- Given a 0-based index i, remove element at that index
-- if index is out of bounds, return the list unchanged
rmElementAtIndex :: [a] -> Int -> [a]
rmElementAtIndex [] _ = []
rmElementAtIndex (first:list) 0 = list
rmElementAtIndex (first:list) i = first : rmElementAtIndex list (i - 1)

-- Find and remove element from list
rmElement :: (Eq a) => a -> [a] -> [a]
rmElement element = Prelude.foldr (\a b -> if a == element  then b else (a:b)) []

-- Add all elements in list A that are not in list B. 
unionList :: (Eq p) => [p] -> [p] -> [p]
unionList [] x = x
unionList x [] = x
unionList (first:listA) listB
   | first `elem` listB = unionList listA listB
   | otherwise = first : unionList listA listB

-- Remove duplicate walls from a grid
rmDuplicateWalls :: [(Integer, Integer, Char)] -> [(Integer, Integer, Char)]
rmDuplicateWalls [] = []
rmDuplicateWalls (wall : walls)
   | elem (getSecondRep wall) walls = rmDuplicateWalls walls
   | otherwise = wall : rmDuplicateWalls walls
   

-- Get the second representation of a wall
getSecondRep :: (Integer, Integer, Char) -> (Integer, Integer, Char)
getSecondRep (r, c, w)
   | w == 'L' = (r, c - 1, 'R')
   | w == 'R' = (r, c + 1, 'L')
   | w == 'U' = (r - 1, c, 'D')
   | w == 'D' = (r + 1, c, 'U')
   | otherwise = (-1, -1, 'X')

