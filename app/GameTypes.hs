module GameTypes where
import System.Random (StdGen)
import Data.Time (UTCTime)
import Shapes (Tetremino)

newtype Block =
  Block {blockColor :: String}

type Row = [Maybe Block]

type Grid = [Row]

gridWidth :: Int
gridWidth = 12

gridHeight :: Int
gridHeight = 22

data GameState = GameState
  { grid :: Grid,
    tetremino :: Tetremino,
    rng :: StdGen,
    lastUpdate :: UTCTime,
    hold :: Maybe Tetremino,
    _score :: Int
  }
