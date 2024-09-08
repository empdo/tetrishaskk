{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Time (UTCTime (utctDayTime), diffUTCTime, getCurrentTime)
import Input
import Shapes
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Random (StdGen, mkStdGen)
import Data.Maybe (isNothing)

newtype Block =
  Block {blockColor :: String}

type Row = [Maybe Block]

type Grid = [Row]

data GameState = GameState
  { grid :: Grid,
    tetremino :: Tetremino,
    rng :: StdGen,
    lastUpdate :: UTCTime
  }

gridWidth :: Int
gridWidth = 12

gridHeight :: Int
gridHeight = 22

handleInput :: Maybe Char -> (Grid, Tetremino) -> Tetremino
handleInput (Just 'a') (_, tetremino) = moveLeft tetremino
handleInput (Just 'd') (_, tetremino) = moveRight tetremino
handleInput (Just 'w') (_, tetremino) = rotateTetremino tetremino
handleInput (Just 's') (grid, tetremino) = hardDropShape tetremino grid Nothing
handleInput _ (_, tetremino) = tetremino

initialState :: IO GameState
initialState = do
  currentTime <- getCurrentTime
  let seed = floor (utctDayTime currentTime) + 123456 -- Adding an offset to the seed for better variability
  let rng = mkStdGen seed
  let (initialTetromino, newRng) = randomTetremino rng
  return
    GameState
      { grid = replicate gridHeight (replicate gridWidth Nothing),
        tetremino = initialTetromino,
        rng = newRng,
        lastUpdate = currentTime
      }

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

isFinalState :: Grid -> Bool
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

clearLines :: Grid -> Grid
clearLines grid =
    let _grid = [ grid !! y | y <- [0..gridHeight-1], any isNothing (grid !! y)]
    in replicate (gridHeight - length _grid) (replicate gridWidth Nothing) ++ _grid

nextState :: GameState -> Maybe Char -> Bool -> GameState
nextState gameState input shouldDrop =
  let -- Handle input to potentially change the Tetromino's position
      movedTet = handleInput input (grid gameState, tetremino gameState)

      -- Check if moving is valid
      validMove = validPos (grid gameState) (pos movedTet)

      -- Determine the new position if the piece should drop
      potentialDrop = moveShape (if validMove then movedTet else tetremino gameState)
      canDrop = validPos (grid gameState) (pos potentialDrop)
      oldRng = rng gameState

      -- Update Tetromino based on dropping logic
      (newTetremino, newRng)
        | shouldDrop =
            if canDrop
              then (potentialDrop, oldRng)
              else randomTetremino oldRng
        | validMove = (movedTet, oldRng)
        | otherwise = (tetremino gameState, oldRng) -- Update grid if the piece has landed
      updatedGrid =
        if shouldDrop && not canDrop
          then placeShape (grid gameState) (tetremino gameState) (pos (tetremino gameState))
          else grid gameState

      clearedGrid = clearLines updatedGrid
   in gameState
        { grid = clearedGrid,
          tetremino = newTetremino,
          rng = newRng
        }

isFinalState = undefined

printGrid :: Grid -> IO [()]
printGrid =
  mapM printRow

printRow :: Row -> IO ()
printRow row = do
  let rowString = "║" ++ concatMap printBlock row ++ "\ESC[30m║"
  putStrLn rowString

printBlock :: Maybe Block -> String
printBlock (Just Block {blockColor = color})
                                            | color == "\ESC[30m" =  color ++ "▒▒"
                                            | otherwise =  color ++ "██"
printBlock Nothing = "\ESC[30m" ++ "・"

shouldMoveDown :: UTCTime -> UTCTime -> Bool
shouldMoveDown currentTime lastTime =
  let diff = diffUTCTime currentTime lastTime
   in diff >= 1

gameLoop :: GameState -> IO [()]
gameLoop gameState = do
  -- Delay and clear screen
  setCursorPosition 0 0

  let tetShape = name (tetremino gameState)
      tetPos = pos (tetremino gameState)
      tetRot = rotPoint (tetremino gameState)

  userInput <- getUserInput

  let ghost = hardDropShape Tetremino {rotPoint=tetRot, pos=tetPos, name=tetShape, color="\ESC[30m"} (grid gameState) Nothing
  let gridWithGhost = placeShape (grid gameState) ghost (pos ghost)
  let gridWithGhostAndPlayerSuperNice = placeShape gridWithGhost (tetremino gameState) tetPos

  putStrLn ("╔" ++ concat (replicate (gridWidth * 2) "═") ++ "╗")
  _ <- printGrid (placeShape gridWithGhostAndPlayerSuperNice ghost (pos ghost))
  putStrLn ("╚" ++ concat (replicate (gridWidth * 2) "═") ++ "╝")

  currentTime <- getCurrentTime
  let _shouldMoveDown = shouldMoveDown currentTime (lastUpdate gameState)
  let _gameState' = nextState gameState userInput _shouldMoveDown
      _timeSinceDown = if _shouldMoveDown then currentTime else lastUpdate gameState

  case userInput of
    Just 'q' -> restoreInput >> return [] -- Quit the game and restore terminal settings
    _ -> gameLoop (_gameState' {lastUpdate = _timeSinceDown}) >> return []

main :: IO [()]
main = do
  clearScreen
  setupInput

  initialState >>= gameLoop
