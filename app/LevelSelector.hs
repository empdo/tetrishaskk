module LevelSelector where
import GameTypes (gridWidth, gridHeight)
import ShapeLogic (printGrid)
import System.Console.ANSI (setCursorPosition, setSGR, SGR (SetConsoleIntensity, Reset), ConsoleIntensity (BoldIntensity))


levelSelector :: IO (Maybe Int)
levelSelector = do

  setCursorPosition 0 0

  let grid = replicate gridHeight (replicate gridWidth Nothing)
  --replace middle line of grid
  putStrLn ("\ESC[37m╔" ++ concat (replicate (gridWidth * 2) "═") ++ "╗")
  _ <- printGrid grid
  putStrLn ("╚" ++ concat (replicate (gridWidth * 2) "═") ++ "╝")

  setCursorPosition 6 10
  setSGR [SetConsoleIntensity BoldIntensity]
  putStrLn "\ESC[37mTETRIS"
  setSGR [Reset]

  setCursorPosition 9 5
  putStrLn "\ESC[95mEnter level: 0-9"

  level <- getLine

  return (read level :: Maybe Int)
  
