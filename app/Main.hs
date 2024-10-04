{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Time (UTCTime (utctDayTime), diffUTCTime, getCurrentTime)
import Input (getUserInput, setupInput, restoreInput, handleInput)
import Shapes
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Random (mkStdGen)
import Data.Maybe (isNothing, fromJust)
import GameTypes (GameState (tetremino, hold, rng, GameState, grid, lastUpdate, _score), Grid, Row, Block (Block, blockColor), gridWidth, gridHeight)
import ShapeLogic (placeShape, hardDropShape, validPos, moveShape, randomTetremino, clearLines, printGrid, printRow)
import LevelSelector (levelSelector)


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
        lastUpdate = currentTime,
        hold = Nothing,
        _score = 0
      }


nextState :: GameState -> Maybe Char -> Bool -> GameState
nextState gameState input shouldDrop =
  let -- Handle input to potentially change the Tetromino's position
      newGamestate = handleInput input gameState
      movedTet = tetremino newGamestate

      -- Check if moving is valid
      validMove = validPos (grid gameState) (pos movedTet)

      -- Determine the new position if the piece should drop
      potentialDrop = moveShape (if validMove then movedTet else tetremino gameState)
      canDrop = validPos (grid gameState) (pos potentialDrop)
      oldRng = rng newGamestate

      -- Update Tetromino based on dropping logic
      (newTetremino, newRng)
        | not canDrop = randomTetremino oldRng
        | shouldDrop = (potentialDrop, oldRng)
        | validMove = (movedTet, oldRng)
        | otherwise = (tetremino gameState, oldRng) -- Update grid if the piece has landed

      updatedGrid =
        if not canDrop
          then placeShape (grid gameState) (tetremino gameState) (pos movedTet)
          else grid gameState

      (clearedGrid, score) = clearLines (updatedGrid, _score newGamestate)
   in gameState
        { grid = clearedGrid,

          tetremino = newTetremino,
          rng = newRng,
          hold = hold newGamestate,
          _score = score 
        }


printSmallGrid :: String -> Maybe Tetremino -> (Int, Int) -> IO ()
printSmallGrid title tetremino (x, y) = do
  let grid = replicate 3 (replicate 4 Nothing)
  let gridlength = length grid
  let _grid = if isNothing tetremino then grid else placeShape grid (fromJust tetremino) (shapeCoordinates (name (fromJust tetremino)))

  setCursorPosition (y - gridlength - 1) x
  putStrLn title
  setCursorPosition (y - gridlength) x
  putStrLn ("╔" ++ concat (replicate ((gridlength + 1) * 2) "═") ++ "╗")
  mapM_ (\(row, y') -> setCursorPosition y' x >> printRow row) (zip _grid [y, y-1..])
  setCursorPosition (y + 1) x
  putStrLn ("╚" ++ concat (replicate ( (gridlength +1) * 2) "═") ++ "╝")

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
  _ <- printGrid gridWithGhostAndPlayerSuperNice
  putStrLn ("╚" ++ concat (replicate (gridWidth * 2) "═") ++ "╝")


  _ <- printSmallGrid "\ESC[37mHold:" (hold gameState) (2*gridWidth + 4, 4)
  setCursorPosition 0 0

  setCursorPosition 6 (2*gridWidth + 4)
  putStrLn $ "\ESC[37mScore: " ++ show (_score gameState)

  currentTime <- getCurrentTime
  let _shouldMoveDown = shouldMoveDown currentTime (lastUpdate gameState)
  let _gameState = nextState gameState userInput _shouldMoveDown
      _timeSinceDown = if _shouldMoveDown then currentTime else lastUpdate gameState

  case userInput of
    Just 'q' -> restoreInput >> return [] -- Quit the game and restore terminal settings
    _ -> gameLoop (_gameState {lastUpdate = _timeSinceDown}) >> return []

main :: IO [()]
main = do
  clearScreen
  setupInput

  level <- levelSelector
  clearScreen

  initialState >>= gameLoop
