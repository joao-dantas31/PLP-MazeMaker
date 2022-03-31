module Maze where

import Data.HashSet
import System.Random

import Utils

-- | Create a maze.
createMaze :: Int -> StdGen -> ([(Integer, Integer, Char)], StdGen)
createMaze gridSize randomGen = (maze, nextGen)
   where gridInteger = num gridSize
         randomMaze = getMaze gridInteger randomGen
         maze = toList $ fst $ randomMaze 
         nextGen = snd $ randomMaze


-- | Wrapper function to get a maze.
getMaze :: Integer -> StdGen -> (HashSet (Integer, Integer, Char), StdGen)
getMaze gridSize randomGen = primsAlgorithm wallList grid cellsSeen gridSize nextGenerator
   where firstRandomCellInt = modRandomNumPair  (num gridSize) $ getPairFromTriplet $ getRandomNumTriplet randomGen
         firstRandomCell = pairIntToNum firstRandomCellInt
         nextGenerator = get3rd $ getRandomNumTriplet randomGen
         cellsSeen = singleton firstRandomCell
         wallList = allCellWalls firstRandomCell
         grid = fromList $ createGrid gridSize


-- | Generate a maze by applying Prim's algorithm
primsAlgorithm :: [(Integer, Integer, Char)]
    -> HashSet (Integer, Integer, Char)
    -> HashSet (Integer, Integer)
    -> Integer
    -> StdGen
    -> (HashSet (Integer, Integer, Char), StdGen)
primsAlgorithm [] grid cellsSeen gridSize randomGenerator = (grid, randomGenerator)
primsAlgorithm wallList grid cellsSeen gridSize randomGen = primsAlgorithm newWallList newGrid newCellsSeen gridSize newRandom
   where randomIndex = fst $ getRandomIndex randomGen $ length wallList
         newRandom = snd $ getRandomIndex randomGen $ length wallList
         wall = wallList !! randomIndex                                              
         intermediateWallList = rmElementAtIndex wallList randomIndex          
         otherWallRep = getSecondRep wall                                        
         newWallList1 = rmElement otherWallRep intermediateWallList         
         otherCell = getCellFromWall otherWallRep                              
         isValidCell = cellInRange otherCell gridSize
         otherSeen = member otherCell cellsSeen   
         allUnseenCellWalls = allCellWalls otherCell
         unseenWalls = rmElement otherWallRep allUnseenCellWalls  
         proceed = isValidCell && not otherSeen                            
         newWallList = if proceed then unionList unseenWalls newWallList1 else newWallList1
         newGrid1 = if proceed then delete wall grid else grid
         newGrid = if proceed then delete otherWallRep newGrid1 else newGrid1
         newCellsSeen = if proceed then insert otherCell cellsSeen else cellsSeen

-- | Represent a maze wall. L is left, R is right, U is upper and D is down.
walls :: [Char]
walls = ['L', 'R', 'U', 'D']

-- | Get the wall boundaries of a cell
allCellWalls :: (Integer, Integer) -> [(Integer, Integer, Char)]
allCellWalls (r, c) = [(r, c, w) | w <- walls]

-- | Create a grid of walls
createGrid :: (Integral a, Num b, Num c) => a -> [(b, c, Char)]
createGrid gridSize = [(num r, num c, w) | r <- [1..gridSize], c <- [1..gridSize], w <- walls]

-- | Check if a cell is in range of the grid
cellInRange :: (Integer, Integer) -> Integer -> Bool
cellInRange (r, c) gridSize = r > 0 && r <= gridSize && c > 0 && c <= gridSize

-- | Check if a wall is an edge wall
isEdgeWall :: Integer -> (Integer, Integer, Char) -> Bool
isEdgeWall gridSize (r, c, w)
   | (w == 'U' && r == 1) = True
   | (w == 'D' && r == gridSize) = True
   | (w == 'L' && c == 1) = True
   | (w == 'R' && c == gridSize) = True
   | otherwise = False

-- | Get the cell surrounded by a wall
getCellFromWall :: (Integer, Integer, Char) -> (Integer, Integer)
getCellFromWall = getPairFromTriplet
