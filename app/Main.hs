{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
module Main where
import Shapes

import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Data.Maybe (fromJust, isNothing, isJust)

data Block where
  Block :: {shape :: Shape} -> Block

type Row = [Maybe Block]
type Grid = [Row]

gridWidth :: Int
gridWidth = 12
gridHeight :: Int
gridHeight = 22

initialState :: (Grid, Tetremino)
initialState = (replicate gridHeight (replicate gridWidth Nothing), randomTetremino)

-- Function to get a random Shape
randomTetremino :: Tetremino
randomTetremino =
  let shape = randomShape
      tetremino = Tetremino {pos=shapeCoordinates shape, name=shape, color=shapeColor shape}
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

moveShape :: [(Int, Int)] -> [(Int, Int)]
moveShape = map (\(x,y) -> (x, y+1))

validPos :: Grid -> [(Int, Int)] -> Bool
validPos grid = all (\(x, y) ->
      x < gridWidth &&
      y < gridHeight &&
      x >= 0 &&
      isNothing ((grid !! y) !! x)
      )


nextState :: (Grid, Tetremino) -> (Grid, Tetremino)
nextState (grid, tetremino) = do
  let newPos = moveShape (pos tetremino)
  let newTet = not (validPos grid newPos)

  let _tetremino
        | newTet = randomTetremino
        | otherwise = Tetremino {pos= newPos, name=name tetremino, color=color tetremino}

  let _grid
        | newTet = placeShape grid (name tetremino) (pos tetremino)
        | otherwise = grid

  (_grid, _tetremino)

isFinalState = undefined

printGrid :: Grid -> IO [()]
printGrid = mapM printRow

printRow :: Row -> IO ()
printRow row = do
    let rowString = concatMap printBlock row
    putStrLn rowString

printBlock :: Maybe Block  -> String
printBlock (Just Block {shape = _shape}) = shapeColor _shape ++ "██"
printBlock Nothing =  "\ESC[30m" ++ "██"

gameLoop :: (Grid, Tetremino) -> IO [()]
gameLoop (grid, tetremino) = do
-- Delay and clear screen
  threadDelay 500000
  setCursorPosition 0 0
  clearScreen

  let tetShape = name tetremino
      tetPos = pos tetremino

  let _grid = placeShape grid tetShape tetPos

  _ <- printGrid _grid

  gameLoop (nextState (grid, tetremino))

main :: IO [()]
main = gameLoop initialState
