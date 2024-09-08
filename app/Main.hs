{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
module Main where
import Shapes
import Input

import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)

data Block where
  Block :: {shape :: Shape} -> Block

type Row = [Maybe Block]
type Grid = [Row]

gridWidth :: Int
gridWidth = 12
gridHeight :: Int
gridHeight = 22

handleInput :: Maybe Char -> Tetremino -> Tetremino
handleInput (Just 'a') tetremino = moveLeft tetremino
handleInput (Just 'd') tetremino = moveRight tetremino
handleInput (Just 'w') tetremino = rotateTetremino tetremino
--handleInput (Just 's') (grid, tetremino) = speedUp (grid, tetremino)
handleInput _ gameState = gameState -- No input or unhandled input, return the state unchanged


initialState :: (Grid, Tetremino)
initialState = (replicate gridHeight (replicate gridWidth Nothing), randomTetremino)

-- Function to get a random Shape
randomTetremino :: Tetremino
randomTetremino =
  let shape = randomShape
      tetremino = Tetremino {pos=shapeCoordinates shape, name=shape, color=shapeColor shape, rotPoint= shapeToRotPoint shape}
  in tetremino

isFinalState :: Grid -> Bool

addBlock :: Grid -> Block -> (Int, Int) -> Grid
addBlock grid block (x, y) =
    let updatedRow = replaceElement (grid !! y) x (Just block)
    in replaceElement grid y updatedRow

replaceElement :: [a] -> Int -> a -> [a]
replaceElement list index element = take index list ++ [element] ++ drop (index + 1) list

placeShape :: Grid -> Shape -> [(Int,Int)] -> Grid
placeShape grid _shape ((x,y):rest) =
    let _grid = addBlock grid Block {shape = _shape} (x,y)
    in placeShape _grid _shape rest
placeShape grid _ _ = grid

moveShape :: Tetremino -> Tetremino
moveShape tetremino = 
    let 
        (rx, ry) = rotPoint tetremino
        newRotPoint = (rx, ry +1)
        newPos = map (\(x,y) -> (x, y+1)) (pos tetremino)
    in Tetremino {pos=newPos, name=name tetremino, color=color tetremino, rotPoint=newRotPoint}

moveLeft :: Tetremino -> Tetremino
moveLeft tetremino = 
    let 
        (rx, ry) = rotPoint tetremino
        newRotPoint = (rx - 1, ry)
        newPos = map (\(x,y) -> (x-1, y)) (pos tetremino)
    in Tetremino {pos=newPos, name=name tetremino, color=color tetremino, rotPoint=newRotPoint}

moveRight :: Tetremino -> Tetremino
moveRight tetremino = 
    let 
        (rx, ry) = rotPoint tetremino
        newRotPoint = (rx+1, ry)
        newPos = map (\(x,y) -> (x+1, y)) (pos tetremino)
    in Tetremino {pos=newPos, name=name tetremino, color=color tetremino, rotPoint=newRotPoint}

rotateTetremino :: Tetremino -> Tetremino
rotateTetremino tetremino = 
    let 
        (rx, ry) = rotPoint tetremino
        newRotPoint = (rx, ry)
        newPos = map (\(x,y) -> (rx + (y-ry), ry - (x-rx))) (pos tetremino)
    in Tetremino {pos=newPos, name=name tetremino, color=color tetremino, rotPoint=newRotPoint}

validPos :: Grid -> [(Int, Int)] -> Bool
validPos grid = all (\(x, y) ->
      x < gridWidth &&
      y < gridHeight &&
      x >= 0 &&
      isNothing ((grid !! y) !! x)
      )

nextState :: (Grid, Tetremino) -> Maybe Char -> Bool -> (Grid, Tetremino)
nextState (grid, tetremino) input shouldDrop =
    let
        -- Handle input to potentially change the Tetromino's position
        movedTet = handleInput input tetremino

        -- Check if moving is valid
        validMove = validPos grid (pos movedTet)

        -- Determine the new position if the piece should drop
        potentialDrop = moveShape (if validMove then movedTet else tetremino)
        canDrop = validPos grid (pos potentialDrop)

        -- Update Tetromino based on dropping logic
        newTetremino
          | shouldDrop = if canDrop then potentialDrop else randomTetremino
          | validMove = movedTet
          | otherwise = tetremino

        -- Update grid if the piece has landed
        updatedGrid = if shouldDrop && not canDrop
            then placeShape grid (name tetremino) (pos tetremino)
            else grid
    in
        (updatedGrid, newTetremino)

isFinalState = undefined

printGrid :: Grid -> IO [()]
printGrid = mapM printRow

printRow :: Row -> IO ()
printRow row = do
    let rowString = concatMap printBlock row
    putStrLn rowString

printBlock :: Maybe Block  -> String
printBlock (Just Block {shape = _shape}) = shapeColor _shape ++ "██"
printBlock Nothing =  "\ESC[30m" ++ " ·"

shouldMoveDown :: UTCTime -> UTCTime -> Bool
shouldMoveDown currentTime lastTime =
  let diff = diffUTCTime currentTime lastTime
  in diff >= 1

gameLoop :: (Grid, Tetremino) -> UTCTime -> IO [()]
gameLoop (grid, tetremino) timeSinceDown = do
-- Delay and clear screen
  setCursorPosition 0 0

  let tetShape = name tetremino
      tetPos = pos tetremino

  userInput <- getUserInput

  let _grid = placeShape grid tetShape tetPos
  _ <- printGrid _grid

  currentTime <- getCurrentTime
  let _shouldMoveDown = shouldMoveDown currentTime timeSinceDown
  let _timeSinceDown = if _shouldMoveDown then currentTime else timeSinceDown


  case userInput of
    Just 'q' -> restoreInput >> return []  -- Quit the game and restore terminal settings
    _ -> gameLoop (nextState (grid, tetremino) userInput _shouldMoveDown) _timeSinceDown >> return []

main :: IO [()]
main = do
  clearScreen
  setupInput

  startTime <- getCurrentTime

  gameLoop initialState startTime
