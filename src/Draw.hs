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
drawWorld w = result
 where

  result = pictures [grid, mconcat boardPieces]
  darkGreen = makeColor 0.1 0.8 0.5 0.6

  grid = pictures
    [color darkGreen $ polygon [(-320, -320), (-320, 320), (320,320), (320,-320)],
    color black (line [ (-240, -320), (-240,  320) ]),
    color black (line [ (-160, -320), (-160,  320) ]),
    color black (line [ (-80, -320), (-80,  320) ]),
    color black (line [ (0, -320), (0,  320) ]),
    color black (line [ (80, -320), (80,  320) ]),
    color black (line [ (160, -320), (160,  320) ]),
    color black (line [ (240, -320), (240,  320) ]),
    color black (line [ (-320, -240), (320,  -240) ]),
    color black (line [ (-320, -160), (320,  -160) ]),
    color black (line [ (-320, -80), (320,  -80) ]),
    color black (line [ (-320, 0), (320, 0) ]),
    color black (line [ (-320, 80), (320,  80) ]),
    color black (line [ (-320, 160), (320,  160) ]),
    color black (line [ (-320, 240), (320,  240) ])]

  boardPieces = mconcat [map drawPiece (pieces (board w))]

drawPiece :: (Position, Col) -> Picture
drawPiece ((x,y), col) = piece

 where
  piece =
   translate (-320.0 + (fromIntegral x) * 80.0 + 40.0)
             (320 - (fromIntegral y) * 80.0 - 40.0) $
        case col of
          White -> color white (thickCircle 1 75)
          Black -> color black (thickCircle 1 75)
