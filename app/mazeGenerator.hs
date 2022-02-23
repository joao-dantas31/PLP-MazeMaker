import Data.Equivalence.Persistent
import System.Random.Shuffle

-- Vertical walls are to the right of their cell (so the x component
-- must be less than width - 1), and horizontal walls are to the top
-- of their cell (so the y component must be less than height - 1).

-- | Represent a maze cell
type Cell = (Int, Int)

-- | Represent a maze wall
data Wall = H Cell | V Cell deriving (Eq, Show)

-- | Given the initial state of a maze, this function process it and build a valid maze.
process [] _ = []
process rooms (H (x, y) : ws)
    -- Process horizontal walls
    | equiv (x, y) (x, y + 1) = H (x, y) : process rooms ws 
    | otherwise = process (equate (x, y) (x, y + 1) rooms) ws
process rooms (V (x, y) : ws)
    -- Process vertical walls
    | equiv (x, y) (x + 1, y) = V (x, y) : process rooms ws
    | otherwise = process (equate (x, y) (x + 1, y) rooms) ws

-- | Given a width, a height and a random generator, this functions generates a maze.
generateMaze :: (RandomGen gen) => Integral -> Integral -> gen -> [Wall]
generateMaze width height gen = maze
    where walls = [ H (x, y) | x  <- [0 .. width - 1], y <- [0 .. height - 2] ] 
                ++ [ V (x, y) | x <- [0 .. width - 2], y <- [0 .. height - 1] ]
          initialRooms = emptyEquivalence ((0, 0), (width - 1, height - 1))
          initialWalls = shuffle' walls (length walls) gen
          maze = process initialRooms initialWalls

