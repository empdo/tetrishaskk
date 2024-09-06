{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
module Main where
import Shapes

import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)

data Block where
  Block :: {shape :: Shape} -> Block

type Row = [Maybe Block]
type Grid = [Row]

gridWidth :: Int
gridWidth = 12
gridHeight :: Int
gridHeight = 22

initialState :: Grid
initialState = replicate gridHeight (replicate gridWidth Nothing)

isFinalState :: Grid -> Bool

addBlock :: Grid -> Block -> (Int, Int) -> Grid
addBlock grid block (x, y) =
    let updatedRow = replaceElement (grid !! y) x (Just block)
    in replaceElement grid y updatedRow

placeShape :: Grid -> Shape -> [(Int,Int)] -> Grid
placeShape grid _shape ((x,y):rest) = 
    let _grid = addBlock grid Block {shape = _shape} (x,y)
    in placeShape _grid _shape rest
placeShape grid _ _ = grid


replaceElement :: [a] -> Int -> a -> [a]
replaceElement list index element = take index list ++ [element] ++ drop (index + 1) list

nextState :: Grid -> Grid
nextState grid = placeShape grid I (shapeCoordinates I)

isFinalState = undefined

printGrid :: Grid -> IO [()]
printGrid = mapM printRow

printRow :: Row -> IO ()
printRow row = do
    let rowString = concatMap printBlock row
    putStrLn rowString

printBlock :: Maybe Block  -> String
printBlock (Just Block {shape = _shape}) = shapeColor _shape ++ "██"
printBlock Nothing =  "\ESC[31m" ++ "██"

gameLoop :: Grid -> IO [()]
gameLoop grid  = do
  threadDelay 100000
  setCursorPosition 0 0
  clearScreen
  _ <- printGrid grid
  gameLoop (nextState grid)

main :: IO [()]
main = gameLoop initialState

{- 
initialState :: World
nextState :: World -> World
isFinalState :: World -> Bool

gameLoop world | isFinalState world = -- ...
               | otherwise = do
                     drawScene world
                     gameLoop (nextState world)

main = gameLoop initialState
-}
