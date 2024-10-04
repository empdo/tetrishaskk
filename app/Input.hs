module Input (handleInput, setupInput, restoreInput, getUserInput) where
import System.IO
    ( hSetBuffering,
      stdin,
      BufferMode(NoBuffering),
      hSetEcho,
      hReady,
      BufferMode(LineBuffering) )
import System.Console.ANSI (hideCursor, showCursor)
import GameTypes (GameState (tetremino, hold, rng, grid))
import Data.Maybe (fromMaybe)
import ShapeLogic (randomTetremino, moveLeft, moveRight, rotateTetremino, hardDropShape)

setupInput :: IO ()
setupInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor

restoreInput :: IO ()
restoreInput = do
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  showCursor

getUserInput :: IO (Maybe Char)
getUserInput = do
  inputAvailable <- hReady stdin
  if inputAvailable
    then Just <$> getChar
    else return Nothing

handleInput :: Maybe Char -> GameState -> GameState
handleInput (Just 'a') gamestate = gamestate {tetremino = moveLeft (tetremino gamestate)}
handleInput (Just 'd') gamestate = gamestate {tetremino = moveRight (tetremino gamestate)}
handleInput (Just 'w') gamestate = gamestate {tetremino = rotateTetremino (tetremino gamestate)}
handleInput (Just 's') gamestate = gamestate {tetremino = hardDropShape (tetremino gamestate) (grid gamestate) Nothing}
handleInput (Just 'h') gamestate =
  let (newTetremino, newRng) = randomTetremino (rng gamestate)
  in gamestate {
    tetremino = fromMaybe newTetremino (hold gamestate),
    hold = Just ( tetremino gamestate),
    rng = newRng
  }
handleInput _ gamestate = gamestate
