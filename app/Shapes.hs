module Shapes (Shape(..), shapeCoordinates, shapeColor) where

-- Define the Tetrimino shapes as a data type
data Shape = J | L | I | S | Z | O | T
            deriving (Eq, Show, Enum)

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
shapeCoordinates J = [(0, 1), (1, 1), (2, 1), (2, 0), (2, 2)]
shapeCoordinates L = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
shapeCoordinates I = [(0, 1), (1, 1), (2, 1), (3, 1)]
shapeCoordinates S = [(0, 1), (1, 1), (1, 0), (2, 0)]
shapeCoordinates Z = [(0, 0), (1, 0), (1, 1), (2, 1)]
shapeCoordinates O = [(0, 0), (0, 1), (1, 0), (1, 1)]
shapeCoordinates T = [(0, 1), (1, 0), (1, 1), (1, 2)]
