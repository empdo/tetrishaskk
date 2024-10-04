module ShapeLogic where
import Shapes (Tetremino (Tetremino, pos, name, color, rotPoint), randomShape, shapeCoordinates, shapeToRotPoint, shapeColor)
import Data.Maybe (isNothing)
import GameTypes (Block(Block, blockColor), Grid, gridWidth, gridHeight, Row)
import System.Random (StdGen)


randomTetremino :: StdGen -> (Tetremino, StdGen)
randomTetremino rng =
  let (shape, newRng) = randomShape rng
      tetremino = Tetremino {pos = shapeCoordinates shape, name = shape, color = shapeColor shape, rotPoint = shapeToRotPoint shape}
   in (tetremino, newRng)


hardDropShape :: Tetremino -> Grid -> Maybe Bool -> Tetremino
hardDropShape tetremino _ (Just False) = tetremino
hardDropShape tetremino grid _ =
  let newPos = moveShape tetremino
      canMove = validPos grid (pos newPos)
   in hardDropShape (if canMove then newPos else tetremino) grid (Just canMove)

addBlock :: Grid -> Block -> (Int, Int) -> Grid
addBlock grid block (x, y) =
  let updatedRow = replaceElement (grid !! y) x (Just block)
   in replaceElement grid y updatedRow

replaceElement :: [a] -> Int -> a -> [a]
replaceElement list index element = take index list ++ [element] ++ drop (index + 1) list

placeShape :: Grid -> Tetremino -> [(Int, Int)] -> Grid
placeShape grid tetremino ((x, y) : rest) =
  let _grid = addBlock grid Block {blockColor = color tetremino} (x, y)
   in placeShape _grid tetremino rest
placeShape grid _ _ = grid

moveShape :: Tetremino -> Tetremino
moveShape tetremino =
  let (rx, ry) = rotPoint tetremino
      newRotPoint = (rx, ry + 1)
      newPos = map (\(x, y) -> (x, y + 1)) (pos tetremino)
   in Tetremino {pos = newPos, name = name tetremino, color = color tetremino, rotPoint = newRotPoint}

moveLeft :: Tetremino -> Tetremino
moveLeft tetremino =
  let (rx, ry) = rotPoint tetremino
      newRotPoint = (rx - 1, ry)
      newPos = map (\(x, y) -> (x - 1, y)) (pos tetremino)
   in Tetremino {pos = newPos, name = name tetremino, color = color tetremino, rotPoint = newRotPoint}

moveRight :: Tetremino -> Tetremino
moveRight tetremino =
  let (rx, ry) = rotPoint tetremino
      newRotPoint = (rx + 1, ry)
      newPos = map (\(x, y) -> (x + 1, y)) (pos tetremino)
   in Tetremino {pos = newPos, name = name tetremino, color = color tetremino, rotPoint = newRotPoint}

rotateTetremino :: Tetremino -> Tetremino
rotateTetremino tetremino =
  let (rx, ry) = rotPoint tetremino
      newRotPoint = (rx, ry)
      newPos = map (\(x, y) -> (rx + (y - ry), ry - (x - rx))) (pos tetremino)
   in Tetremino {pos = newPos, name = name tetremino, color = color tetremino, rotPoint = newRotPoint}

validPos :: Grid -> [(Int, Int)] -> Bool
validPos grid =
  all
    ( \(x, y) ->
        x < gridWidth
          && y < gridHeight
          && x >= 0
          && isNothing ((grid !! y) !! x)
    )


getScore :: Int -> Int -> Int
getScore lines level 
      | lines == 1 = 40* (level + 1)
      | lines == 2 = 100 * (level + 1)
      | lines == 3 = 300 * (level + 1)
      | lines == 4 = 1200 * (level + 1)
      | otherwise = 0

clearLines :: (Grid, Int) -> (Grid, Int)
clearLines (grid, score) =
    let _grid = [ grid !! y | y <- [0..gridHeight-1], any isNothing (grid !! y)]
    in (replicate (gridHeight - length _grid) (replicate gridWidth Nothing) ++ _grid, 
    getScore (gridHeight - length _grid) 0 + score)

printGrid :: Grid -> IO [()]
printGrid =
  mapM printRow

printRow :: Row -> IO ()
printRow row = do
  let rowString = "\ESC[37m║" ++ concatMap printBlock row ++ "\ESC[37m║"
  putStrLn rowString

printBlock :: Maybe Block -> String
printBlock (Just Block {blockColor = color})
                                            | color == "\ESC[30m" =  color ++ "▒▒"
                                            | otherwise =  color ++ "██"
printBlock Nothing = "\ESC[30m" ++ "・"

