module Draw(drawWorld) where

import Graphics.Gloss
import Data.Monoid
import Board

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = if game_over w then drawGameOver w else drawBoard w

drawGameOver :: World -> Picture
drawGameOver w = do result
                 where result = Pictures[grid, text]
                       sizeOfCell = 80
                       gridSize = size (board w) * sizeOfCell

                       darkGreen = makeColor 0.1 0.8 0.5 0.6
                       grid = color darkGreen $ rectangleSolid (fromIntegral(gridSize)) (fromIntegral(gridSize))

                       text = Text "Game Over"

drawBoard :: World -> Picture
drawBoard w = result
              where
               result = pictures [grid, mconcat boardPieces, mconcat lines]
               darkGreen = makeColor 0.1 0.8 0.5 0.6

               sizeOfCell = 80
               gridSize = size (board w) * sizeOfCell
               lineCoords = [(-gridSize + sizeOfCell), (-gridSize + (sizeOfCell * 2))..(gridSize - sizeOfCell)]

               grid = color darkGreen $ rectangleSolid (fromIntegral(gridSize)) (fromIntegral(gridSize))

               boardPieces = map (drawPiece (gridSize) (sizeOfCell)) (pieces (board w))
               lines = map (drawLine (gridSize)) lineCoords

drawPiece :: Int -> Int -> (Position, Col) -> Picture
drawPiece gridSize sizeOfCell ((x,y), col) = piece

 where

  sizeOfCellFloat = fromIntegral (sizeOfCell)
  gridSizeFloat = fromIntegral (gridSize)

  piece =
   translate ((-gridSizeFloat * 0.5) + ((fromIntegral x) * sizeOfCellFloat) + (0.5 * sizeOfCellFloat))
             ((gridSizeFloat * 0.5) - ((fromIntegral y) * sizeOfCellFloat) - (0.5 * sizeOfCellFloat)) $
        case col of
          White -> color white (thickCircle 1 75)
          Black -> color black (thickCircle 1 75)

drawLine :: Int -> Int -> Picture
drawLine gridSize coordinate = pictures [line1, line2]

 where

  coordinateFloat = fromIntegral (coordinate)
  gridSizeFloat = fromIntegral (gridSize)

  line1 =
   color black (line [ (coordinateFloat, -gridSizeFloat), (coordinateFloat, gridSizeFloat) ])
  line2 =
   color black (line [ (-gridSizeFloat, coordinateFloat), (gridSizeFloat, coordinateFloat) ])
