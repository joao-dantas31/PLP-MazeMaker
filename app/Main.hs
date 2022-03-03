-- Copy of https://github.com/bergsans/glossy-haskell-game as base
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import Maze
import System.Random
import System.Environment
import Data.Int

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
    , generator :: StdGen
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

goalPosition :: (Float, Float)
goalPosition = (fromIntegral(fst winSize) / 2 - 26, fromIntegral(snd winSize) / 2 - 26)

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
prepareData = map makeRow

whatImg :: Cell -> Picture -> Picture -> Picture
whatImg (point, cellType) tile1 tile2
  | cellType == 'V' = tile1
  | otherwise = tile2

drawTile :: Cell -> Picture -> Picture  -> Picture
drawTile cell tileImg1 tileImg2 =
  uncurry translate (fst cell) (whatImg cell tileImg1 tileImg2)

render :: GameState -> [Picture] -> Picture
render gs imgs =
  pictures
    ([drawTile cell (head imgs) (imgs !! 1) | cell <- currentLevel gs] ++
     [ uncurry translate (position gs)
         (imgs !! 3)
     ] ++
     [ uncurry translate goalPosition
         (imgs !! 2)
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
  gs {position = initialPosition, currentLevel = prepareData level, generator = gen}
    where (level, gen) = createMaze 25 (generator gs)
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

isHitGoal :: Point -> Point -> Bool
isHitGoal (b1x, b1y) (b2x, b2y) =
    b1x + 10 > b2x - 10  && b1x - 10 < b2x + 10 &&
    b1y + 10 > b2y - 10 && b1y - 10 < b2y + 10

isCollision :: GameState -> Point -> [CellType] -> Bool
isCollision gs pnt checkType =
  any
    (\((x, y), tileType) -> tileType `elem` checkType && isHit pnt (x, y) tileType)
    (currentLevel gs)

moveX :: MoveDirection -> GameState -> Point
moveX East gs =
  if not
        (isCollision
          gs
          (fst (position gs) + speedX gs, snd (position gs))
          ['H', 'V'])
    then (fst (position gs) + speedX gs, snd (position gs))
    else position gs
moveX West gs =
  if not
       (isCollision
          gs
          (fst (position gs) + speedX gs * (-1), snd (position gs))
          ['H', 'V'])
    then (fst (position gs) + speedX gs * (-1), snd (position gs))
    else position gs
moveX _ gs = position gs

moveY :: MoveDirection -> GameState -> Point -> Point
moveY North gs pnt =
  if not
       (isCollision
          gs
          (fst pnt, snd pnt + speedY gs)
          ['H', 'V'])
    then (fst pnt, snd pnt + speedY gs)
    else pnt
moveY South gs pnt =
  if not
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
    , position = pos
    , currentLevel = actualLevel
    , generator = gen
    }
    where fingCase = isHitGoal (position gs) goalPosition
          (level, gen) = if fingCase then createMaze size (generator gs) else ([], generator gs)
          actualLevel = if fingCase then prepareData level else currentLevel gs
          pos = if fingCase then initialPosition else moveY (direction gs) gs $ moveX (direction gs) gs

getRandomGen :: Int -> StdGen
getRandomGen = mkStdGen

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
  seed <- randomIO :: IO Int
  let (level, gen) = createMaze 25 (getRandomGen seed)
  let state =
        GameState
          { position = initialPosition
          , direction = None
          , currentLevel = prepareData level
          , speedX = 0
          , speedY = 0
          , generator = gen
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
