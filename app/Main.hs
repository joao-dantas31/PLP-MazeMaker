-- Copy of https://github.com/bergsans/glossy-haskell-game as base
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import Maze
import System.Random

data MoveDirection
  = East
  | West
  | North
  | South
  | None
  deriving (Eq)

data GameState =
  GameState
    { position :: Point
    , direction :: MoveDirection
    , currentLevel :: Level
    , speedX :: Float
    , speedY :: Float
    }

type CellType = Char

type Cell = (Point, CellType)

type Level = [Cell]

tileSize :: Float
tileSize = 10

size :: Int
size = 25

winSize :: (Int, Int)
winSize = (850, 850)

offset :: Float
offset = fromIntegral (snd winSize-20) / 2

initialPosition :: (Float, Float)
initialPosition = ((fromIntegral(fst winSize) / 2 - 26) * (-1), (fromIntegral(snd winSize) / 2 - 26) * (-1))

window :: Display
window = InWindow "Mazemaker" winSize (0, 0)

background :: Color
background = makeColor 0.9 0.9 0.9 1

fps :: Int
fps = 60

makeRow :: (Integer, Integer, Char) -> Cell
makeRow (x,y,dir)
   | dir == 'U' = (
                    (c*(fromIntegral y-1)+c/2-offset, h-c*(fromIntegral x-1)-offset)
                  , 'V')
   | dir == 'D' = (
                    (c*(fromIntegral y-1)+c/2-offset, h-c*fromIntegral x-offset)
                  , 'V')
   | dir == 'L' = (
                    (c*(fromIntegral y-1)-offset, h-c/2-c*(fromIntegral x-1)-offset)
                  , 'H')
   | dir == 'R' = (
                    (c*fromIntegral y-offset, h-c/2-c*(fromIntegral x-1)-offset)
                  , 'H')
   | otherwise = ((0,0), 'f')
      where
         c = scaleFactor (fromIntegral size)
         h = c * fromIntegral size

prepareData :: [(Integer, Integer, Char)] -> Level
prepareData maze = map makeRow (removeDuplicateWalls maze)

whatImg :: Cell -> Picture -> Picture -> Picture -> Picture
whatImg (point, cellType) tile1 tile2 obj
  | cellType == 'V' = tile1
  | cellType == 'H' = tile2
  | otherwise = obj
      where
         c = scaleFactor (fromIntegral size)

drawTile :: Cell -> Picture -> Picture -> Picture -> Picture
drawTile cell tileImg1 tileImg2 caseImg =
  uncurry translate (fst cell) (whatImg cell tileImg1 tileImg2 caseImg)

render :: GameState -> [Picture] -> Picture
render gs imgs =
  pictures
    ([drawTile cell (head imgs) (imgs !! 1) (imgs !! 2) | cell <- currentLevel gs] ++
     [ uncurry translate (position gs)
         (imgs !! 3)
     ])

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs {direction = West}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs {direction = East}
handleKeys (EventKey (SpecialKey KeyUp ) Down _ _) gs =
  gs {direction = North}
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) gs =
  gs {direction = South}
handleKeys (EventKey ((Char '1') ) Down _ _) gs =
  gs {position = initialPosition, currentLevel = prepareData (createMaze size (getRandomGen 8))}
handleKeys _ gs = gs {direction = None}

checkSpeedY :: GameState -> Float
checkSpeedY gs
  | direction gs == North || direction gs == South =
    if speedY gs > 5.0
      then 5.0
      else speedY gs + 0.5
  | otherwise = 0

checkSpeedX :: GameState -> Float
checkSpeedX gs
  | direction gs == West || direction gs == East =
    if speedX gs > 5.0
      then 5.0
      else speedX gs + 0.5
  | otherwise = 0


isHit :: Point -> Point -> CellType -> Bool
isHit (b1x, b1y) (b2x, b2y) tipo =
    b1x + 10 > b2x - width  && b1x - 10 < b2x + width &&
    b1y + 10 > b2y - height && b1y - 10 < b2y + height
    where
      (width, height) = if tipo == 'H' then (2, 18) else (18, 2)

isCollision :: GameState -> Point -> [CellType] ->[Cell]
isCollision gs pnt checkType =
  filter
    (\((x, y), tileType) -> tileType `elem` checkType && isHit pnt (x, y) tileType)
    (currentLevel gs)

moveX :: MoveDirection -> GameState -> Point
moveX East gs =
  if null
        (isCollision
          gs
          (fst (position gs) + speedX gs, snd (position gs))
          ['H', 'V'])
    then (fst (position gs) + speedX gs, snd (position gs))
    else position gs
moveX West gs =
  if null
       (isCollision
          gs
          (fst (position gs) + speedX gs * (-1), snd (position gs))
          ['H', 'V'])
    then (fst (position gs) + speedX gs * (-1), snd (position gs))
    else position gs
moveX _ gs = position gs

moveY :: MoveDirection -> GameState -> Point -> Point
moveY North gs pnt =
  if null
       (isCollision
          gs
          (fst pnt, snd pnt + speedY gs)
          ['H', 'V'])
    then (fst pnt, snd pnt + speedY gs)
    else pnt
moveY South gs pnt =
  if null
       (isCollision
          gs
          (fst pnt, snd pnt  + speedY gs * (-1))
          ['H', 'V'])
    then (fst pnt, snd pnt  + speedY gs * (-1))
    else pnt
moveY _ gs pnt = pnt

update :: Float -> GameState -> GameState
update _ gs =
  gs
    { speedY = checkSpeedY gs
    , speedX = checkSpeedX gs
    , position = moveY (direction gs) gs $ moveX (direction gs) gs
    }

getRandomGen :: Int -> StdGen
getRandomGen = mkStdGen

removeDuplicateWalls :: [(Integer, Integer, Char)] -> [(Integer, Integer, Char)]
removeDuplicateWalls [] = []
removeDuplicateWalls (wall : walls)
   | getSecondRep wall `elem` walls = removeDuplicateWalls walls
   | otherwise = wall : removeDuplicateWalls walls

getSecondRep :: (Integer, Integer, Char) -> (Integer, Integer, Char)
getSecondRep (r, c, w)
   | w == 'L' = (r, c - 1, 'R')
   | w == 'R' = (r, c + 1, 'L')
   | w == 'U' = (r - 1, c, 'D')
   | w == 'D' = (r + 1, c, 'U')
   | otherwise = (-1, -1, 'X')

wall :: Float -> Float -> Picture
wall w h = color black (rectangleSolid w h)

scaleFactor :: Float -> Float
scaleFactor size = fromIntegral(fst winSize-20) / size

main :: IO ()
main = do
  lase1Img <- loadBMP "assets/lazer1.bmp"
  lase2Img <- loadBMP "assets/lazer2.bmp"
  caseImg <- loadBMP "assets/case.bmp"
  char <- loadBMP "assets/spy.bmp"
  rawData <- readFile "assets/level"
  let level = prepareData (createMaze 25 (getRandomGen 13))
  let state =
        GameState
          { position = initialPosition
          , direction = None
          , currentLevel = level
          , speedX = 0
          , speedY = 0
          }
  play
    window
    background
    fps
    state
    (`render` [ lase1Img
              , lase2Img
              , caseImg
              , char
              ])
    handleKeys
    update
