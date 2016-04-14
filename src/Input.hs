module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
--	= w{ board = (board w){pieces = ((1,1), Black) : pieces (board w)} } this will update the picture, just for testing.
    = if (turn w == White) then case function x y w of
-- if nothing, return the old world
        Nothing -> trace ("Invalid Move") w
-- if the function returned a board, show the x, y and update the board also update the turn.
        Just b  -> trace ("Left button pressed at: " ++ show (x,y)) w{ board = b, turn = other (turn w) }
            else trace("AI turn") w
handleInput e w  = w

function :: Float -> Float ->  World -> Maybe Board
function x y w = makeMove (board w) (turn w) (x',y')
    where
-- floor(x(y)/80) will return an integer from -4 to 3, add half of the board size to get the position in board.
-- for example, x:-20 y:-20 will return          x':-1+4=3      y':4-0 =4 which is the bottom left white piece at (3,4)
	x' = floor (x / 80.0) + (size (board w) `div` 2)
	y' = (size (board w) `div` 2)- ceiling (y/80.0)



{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.
 At first, it is reasonable to assume that both players are human players.
-}
