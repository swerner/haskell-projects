module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Data.Vector as V

width, height, offset :: Int
width = 400
height = 400
offset = 100

data ConwayGame = Game
  { gameBoard :: V.Vector CellState } deriving Show

type Radius = Float
type Position = (Float, Float)
type CellState = Bool

initialState :: ConwayGame
initialState = Game
  { gameBoard =
      V.replicate 400 False V.// [(44, True)]
  }

render :: ConwayGame -> Picture
render game =
  pictures
    [hLines, vLines, cells]
  where
    cells = pictures $ V.toList $ V.imap translateToPiece $ gameBoard game

    translateToPiece :: Int -> CellState -> Picture
    translateToPiece index False = translate (-300) (-300) $ color black $ circleSolid 9
    translateToPiece index state
      | index < 1 = translate (-190) (-190) $ color blue $ circleSolid 9
      | otherwise = translate (-190 + (fromIntegral xPos * 20)) (-190 + (fromIntegral yPos * 20)) $ color blue $ circleSolid 9
      where
        xPos = index `mod` 10
        yPos = index `quot` 10

    hLines = pictures
      [ color red $ line [(-200, -200), (200, -200)]
      , color red $ line [(-200, -180), (200, -180)]
      , color red $ line [(-200, -160), (200, -160)]
      , color red $ line [(-200, -140), (200, -140)]
      , color red $ line [(-200, -120), (200, -120)]
      , color red $ line [(-200, -100), (200, -100)]
      , color red $ line [(-200,  -80), (200,  -80)]
      , color red $ line [(-200,  -60), (200,  -60)]
      , color red $ line [(-200,  -40), (200,  -40)]
      , color red $ line [(-200,  -20), (200,  -20)]
      , color red $ line [(-200,    0), (200,    0)]
      , color red $ line [(-200,   20), (200,   20)]
      , color red $ line [(-200,   40), (200,   40)]
      , color red $ line [(-200,   60), (200,   60)]
      , color red $ line [(-200,   80), (200,   80)]
      , color red $ line [(-200,  100), (200,  100)]
      , color red $ line [(-200,  120), (200,  120)]
      , color red $ line [(-200,  140), (200,  140)]
      , color red $ line [(-200,  160), (200,  160)]
      , color red $ line [(-200,  180), (200,  180)]
      , color red $ line [(-200,  200), (200,  200)]
      ]
    vLines = pictures
      [ color red $ line [(-200, -200), (-200, 200)]
      , color red $ line [(-180, -200), (-180, 200)]
      , color red $ line [(-160, -200), (-160, 200)]
      , color red $ line [(-140, -200), (-140, 200)]
      , color red $ line [(-120, -200), (-120, 200)]
      , color red $ line [(-100, -200), (-100, 200)]
      , color red $ line [(-80,  -200), (-80,  200)]
      , color red $ line [(-60,  -200), (-60,  200)]
      , color red $ line [(-40,  -200), (-40,  200)]
      , color red $ line [(-20,  -200), (-20,  200)]
      , color red $ line [(0,    -200), (0,    200)]
      , color red $ line [(20,   -200), (20,   200)]
      , color red $ line [(40,   -200), (40,   200)]
      , color red $ line [(60,   -200), (60,   200)]
      , color red $ line [(80,   -200), (80,   200)]
      , color red $ line [(100,  -200), (100,  200)]
      , color red $ line [(120,  -200), (120,  200)]
      , color red $ line [(140,  -200), (140,  200)]
      , color red $ line [(160,  -200), (160,  200)]
      , color red $ line [(180,  -200), (180,  200)]
      , color red $ line [(200,  -200), (200,  200)]
      ]

window :: Display
window = InWindow "Game of Life" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60
main :: IO ()
main = display window background $ render initialState

