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
      V.replicate 400 False V.// [(190, True), (211, True), (229, True), (230, True), (231, True)]
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
      | index < 1 = translate (-190) 190 $ color blue $ circleSolid 9
      | otherwise = translate (-190 + (fromIntegral xPos * 20)) (190 - (fromIntegral yPos * 20)) $ color blue $ circleSolid 9
      where
        xPos = index `mod` 20
        yPos = index `quot` 20

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

stepGame :: Float -> ConwayGame -> ConwayGame
stepGame seconds game = game { gameBoard = (processBoard $ gameBoard game)}
  where
    processBoard :: V.Vector CellState -> V.Vector CellState
    processBoard gameBoard = V.imap nextState gameBoard
      where
        nextState :: Int -> CellState -> CellState
        nextState index state
          | liveNeighbors < 2 && state == True = False
          | (liveNeighbors == 2 || liveNeighbors == 3) && state == True = True
          | liveNeighbors > 3 = False
          | liveNeighbors == 3 = True
          | otherwise = False
          where
            liveNeighbors = length (filter (== True) [topLeftDiag, topMid, topRightDiag, left, right, bottomLeftDiag, bottomMid, bottomRightDiag])
            topLeftDiag = if index < 21 then False else (gameBoard V.! (index - 21))
            topMid = if index < 20 then False else (gameBoard V.! (index - 20))
            topRightDiag = if index < 19 then False else (gameBoard V.! (index - 19))
            left = if index < 1 then False else (gameBoard V.! (index - 1))
            right = if index > 398 then False else (gameBoard V.! (index + 1))
            bottomLeftDiag = if index > 379 then False else (gameBoard V.! (index + 19))
            bottomMid = if index > 378 then False else (gameBoard V.! (index + 20))
            bottomRightDiag = if index > 377 then False else (gameBoard V.! (index + 21))

fps :: Int
fps = 10

main :: IO ()
main = simulate window background fps initialState render update

update :: ViewPort -> Float -> ConwayGame -> ConwayGame
update _ = stepGame

