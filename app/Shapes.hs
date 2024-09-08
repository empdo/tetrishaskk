module Shapes (Shape(..), shapeCoordinates, shapeColor, Tetremino(..), randomShape, shapeToRotPoint) where
import System.Random (RandomGen, randomR, mkStdGen)

import Data.Time.Clock.POSIX (getPOSIXTime)

data Tetremino = Tetremino {name :: Shape, color :: String, pos :: [(Int, Int)], rotPoint :: (Int, Int)} deriving (Eq, Show)

-- Define the Tetrimino shapes as a data type
data Shape = J | L | I | S | Z | O | T
  deriving (Eq, Show, Enum, Bounded)

randomShape' :: RandomGen g => g -> (Shape, g)
randomShape' gen =
  let (index, newGen) = randomR (fromEnum (minBound :: Shape), fromEnum (maxBound :: Shape)) gen
  in (toEnum index, newGen)

randomShape :: Shape
randomShape =
  let (shape,_) = randomShape' (mkStdGen 30)
  in shape

currentUTCTimeAsInt :: IO Int
currentUTCTimeAsInt = do
  posixTime <- getPOSIXTime
  return (floor posixTime :: Int)

-- Function to convert a shape to its corresponding grid (list of lists)
shapeColor :: Shape -> String
shapeColor J = "\ESC[34m"   -- Blue
shapeColor L = "\ESC[33m"   -- Yellow
shapeColor I = "\ESC[36m"   -- Cyan
shapeColor S = "\ESC[32m"   -- Green
shapeColor Z = "\ESC[31m"   -- Red
shapeColor O = "\ESC[35m"   -- Magenta
shapeColor T = "\ESC[37m"   -- White

shapeCoordinates :: Shape -> [(Int, Int)]
shapeCoordinates J = [(0, 1), (1, 1), (2, 1), (2, 2)]
shapeCoordinates L = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
shapeCoordinates I = [(0, 1), (1, 1), (2, 1), (3, 1)]
shapeCoordinates S = [(0, 1), (1, 1), (1, 0), (2, 0)]
shapeCoordinates Z = [(0, 0), (1, 0), (1, 1), (2, 1)]
shapeCoordinates O = [(0, 0), (0, 1), (1, 0), (1, 1)]
shapeCoordinates T = [(0, 1), (1, 0), (1, 1), (1, 2)]


shapeToRotPoint :: Shape -> (Int, Int)
shapeToRotPoint J = (1, 1)
shapeToRotPoint L = (1, 1)
shapeToRotPoint I = (1, 2)
shapeToRotPoint O = (0, 0)  -- O shape has no rotation, so the pivot is typically the top-left block
shapeToRotPoint S = (1, 1)
shapeToRotPoint T = (1, 1)
shapeToRotPoint Z = (1, 1)
