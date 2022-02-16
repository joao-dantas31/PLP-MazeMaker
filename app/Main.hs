-- Copy of https://github.com/bergsans/glossy-haskell-game as base
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

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
tileSize = 32.0

window :: Display
window = InWindow "Mazemaker" (1568, 800) (0, 0)

background :: Color
background = makeColor 0.2 0.1 0.1 1

fps :: Int
fps = 60

isHit :: Point -> Point -> Bool
isHit (b1x, b1y) (b2x, b2y) =
  (b1x - 10) < b2x + tileSize &&
  b1x + 50 - 10 > b2x && b1y < b2y + tileSize && b1y + 54 > b2y

makeRow :: String -> Int -> Level
makeRow row y =
  [ ( ( (fromIntegral x * tileSize) - ((1568 / 2) - (tileSize / 2))
      , (fromIntegral y * tileSize) - ((800 / 2) - (tileSize / 2)))
    , row !! x)
  | x <- [0 .. length row - 1]
  , row !! x == '#' || row !! x == '*'
  ]

prepareData :: [String] -> Level
prepareData rawData =
  concat [makeRow (rawData !! y) y | y <- [0 .. length rawData - 1]]

whatImg :: Cell -> Picture -> Picture -> Picture
whatImg (_, cellType) tile obj =
  if cellType == '#'
    then tile
    else obj

drawTile :: Cell -> Picture -> Picture -> Picture
drawTile cell tileImg caseImg =
  uncurry translate (fst cell) (whatImg cell tileImg caseImg)

render :: GameState -> [Picture] -> Picture
render gs imgs =
  pictures
    ([drawTile cell (head imgs) (imgs !! 1) | cell <- currentLevel gs] ++
     [ translate
         (fst (position gs))
         (snd (position gs) + 10)
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

isCollision :: GameState -> Point -> CellType -> Bool
isCollision gs pnt checkType =
  any
    (\((x, y), tileType) -> tileType == checkType && isHit pnt (x, y))
    (currentLevel gs)

moveX :: MoveDirection -> GameState -> Point
moveX East gs =
  if not (isCollision gs (fst (position gs) + speedX gs, snd (position gs)) '#')
    then (fst (position gs) + speedX gs, snd (position gs))
    else position gs
moveX West gs =
  if not
       (isCollision
          gs
          (fst (position gs) + speedX gs * (-1), snd (position gs))
          '#')
    then (fst (position gs) + speedX gs * (-1), snd (position gs))
    else position gs
moveX _ gs = position gs

moveY :: MoveDirection -> GameState -> Point -> Point
moveY North gs pnt =
  if not
       (isCollision
          gs
          (fst pnt, snd pnt + speedY gs)
          '#')
    then (fst pnt, snd pnt + speedY gs)
    else pnt
moveY South gs pnt =
  if not
       (isCollision
          gs
          (fst pnt, snd pnt  + speedY gs * (-1))
          '#')
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

main :: IO ()
main = do
  tileImg <- loadBMP "assets/tile.bmp"
  caseImg <- loadBMP "assets/case.bmp"
  char <- loadBMP "assets/spy.bmp"
  rawData <- readFile "assets/level"
  let level = prepareData $ reverse $ lines rawData
  let state =
        GameState
          { position = (0.0, 325.0)
          , direction = None
          , currentLevel = level
          , speedX = 0
          , speedY = (-6)
          }
  play
    window
    background
    fps
    state
    (`render` [ tileImg
              , caseImg
              , char
              ])
    handleKeys
    update
