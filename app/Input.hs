module Input where
import System.IO
    ( hSetBuffering,
      stdin,
      BufferMode(NoBuffering),
      hSetEcho,
      hReady,
      BufferMode(LineBuffering) )
import System.Console.ANSI (hideCursor, showCursor)

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


